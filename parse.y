%{
#define YYERROR_VERBOSE
#define YYSTYPE struct node *
struct node;
extern int yylex();
extern void yyerror(char const *s);
extern struct node *mk_node(char const *name, int n, ...);
extern struct node *mk_atom(char *text);
extern struct node *mk_none();
extern struct node *ext_node(struct node *nd, int n, ...);
extern void push_back(char c);
extern char *yytext;
%}
%debug

%token DIGIT

%token SLASHEQ
%token DOTDOT
%token DOTDOTDOT
%token ANDEQ
%token ANDAND
%token OREQ
%token OROR
%token MINUSEQ
%token DECREMENT
%token PLUSEQ
%token INCREMENT
%token LE
%token LSH
%token LSHEQ
%token LSHRSH
%token LSHRSHEQ
%token RE
%token RSHEQ
%token RSHRSHEQ
%token RSH
%token RSHRSH
%token NOTEQ
%token NOTLERE
%token NOTLEREEQ
%token NOTLE
%token NOTLEEQ
%token NOTRE
%token NOTREEQ
%token EQEQ
%token STAREQ
%token PERCENTEQ
%token CARETEQ
%token CARETCARET
%token CARETCARETEQ
%token TILDEEQ

%token INTEGER
%token FLOATINGPOINT
%token HEXSTRING
%token WYSIWYGSTRING
%token DOUBLEQOUTEDSTRING
%token TOKENSTRING

// Keywords.
%token ABSTRACT
%token ALIAS
%token ALIGN
%token ASM
%token ASSERT
%token AUTO
%token BODY
%token BOOL
%token BREAK
%token BYTE
%token CASE
%token CAST
%token CATCH
%token CDOUBLE
%token CENT
%token CFLOAT
%token CHAR
%token CLASS
%token CONST
%token CONTINUE
%token CREAL
%token DCHAR
%token DEBUG
%token DEFAULT
%token DELEGATE
%token DELETE
%token DEPRECATED;
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXPORT
%token EXTERN
%token FALSE
%token FINAL
%token FINALLY
%token FLOAT
%token FOR
%token FOREACH
%token FOREACHREVERSE
%token FUNCTION
%token GOTO
%token IDOUBLE
%token IF
%token IFLOAT
%token IMMUTABLE
%token IMPORT
%token IN
%token INOUT
%token INT
%token INTERFACE
%token INVARIANT
%token IREAL
%token IS
%token LAZY
%token LONG
%token MACRO
%token MIXIN
%token MODULE
%token NEW
%token NOTHROW
%token NULLKEYWORD
%token OUT
%token OVERRIDE
%token PACKAGE
%token PRAGMA
%token PRIVATE
%token PROTECTED
%token PUBLIC
%token PURE
%token REAL
%token REF
%token RETURN
%token SCOPE
%token SHARED
%token SHORT
%token STATIC
%token STRUCT
%token SUPER
%token SWITCH
%token SYNCHRONIZED
%token TEMPLATE
%token THIS
%token THROW
%token TRUE
%token TRY
%token TYPEDEF
%token TYPEID
%token TYPEOF
%token UBYTE
%token UCENT
%token UINT
%token ULONG
%token UNION
%token UNITTEST
%token USHORT
%token VERSION
%token VOID
%token VOLATILE
%token WCHAR
%token WHILE
%token WITH

%token SPECIAL_FILE
%token SPECIAL_FILEFULLPATH
%token SPECIAL_MODULE
%token SPECIAL_LINE
%token SPECIAL_FUNCTION
%token SPECIAL_PRETTYFUNCION
%token SPECIAL_GSHARED
%token SPECIAL_TRAITS
%token SPECIAL_VECTOR
%token SPECIAL_PARAMETERS

// Traits Keywords.
%token TRAITS_ISABSTRACTCLASS
%token TRAITS_ISARITHMETIC
%token TRAITS_ISASSOCIATIVEARRAY
%token TRAITS_ISFINALCLASS
%token TRAITS_ISPOD
%token TRAITS_ISNESTED
%token TRAITS_ISFLOATING
%token TRAITS_INTEGRAL
%token TRAITS_ISSCALAR
%token TRAITS_ISSTATICARRAY
%token TRAITS_ISUNSIGNED
%token TRAITS_ISVIRTUALFUNCTION
%token TRAITS_ISVIRTUALMETHOD
%token TRAITS_ISABSTRACTFUNCTION
%token TRAITS_ISFINALFUNCTION
%token TRAITS_ISSTATICFUNCTION
%token TRAITS_ISOVERRIDEFUNCTION
%token TRAITS_ISTEMPLATE
%token TRAITS_ISREF
%token TRAITS_ISOUT
%token TRAITS_ISLAZY
%token TRAITS_HASMANY
%token TRAITS_IDENTIFIER
%token TRAITS_GETALIASTHIS
%token TRAITS_GETATTRIBUTES
%token TRAITS_GETFUNCTIONATTRIBUTES
%token TRAITS_GETFUNCTIONVARIADICSTYLE
%token TRAITS_GETLINKAGE
%token TRAITS_GETMEMBER
%token TRAITS_GETOVERLOADS
%token TRAITS_GETPARAMETERSTORAGECLASSES
%token TRAITS_GETPOINTERBITMAP
%token TRAITS_GETPROTECTION
%token TRAITS_GETVIRTUALFUNCTIONS
%token TRAITS_GETVIRTUALMETHODS
%token TRAITS_UNITTEST
%token TRAITS_PARENT
%token TRAITS_CLASSINSTANCESIZE
%token TRAITS_GETVIRTUALINDEX
%token TRAITS_ALLMEMBERS
%token TRAITS_DERIVEDMEMBERS
%token TRAITS_ISSAME
%token TRAITS_COMPILES

// Property identifier.
%token PROPERTY
%token SAFE
%token TRUSTED
%token SYSTEM
%token DISABLE
%token NOGC

// Lambda expression.
%token EQRSH

%token IDENTIFIER

%start start

%%

/**
Start.
*/
start:
                type
        |       statement
        |       expression
        |       declaration
        |       module
                ;

/**
Type.
*/

type:
                basic_type { $$ = mk_node("Type", 1, $1); }
        |       type_ctors basic_type { $$ = mk_node("Type", 2, $1, $2); }
        |       basic_type basic_type2 { $$ = mk_node("Type", 2, $1, $2); }
        |       type_ctors basic_type basic_type2 { $$ = mk_node("Type", 3, $1, $2, $3); }
                ;

type_ctors:
                type_ctor { $$ = mk_node("TypeCtors", 1, $1); }
        |       type_ctor type_ctors { $$ = ext_node($1, 1, $2); }
                ;

type_ctor:
                CONST { $$ = mk_atom(yytext); }
        |       IMMUTABLE { $$ = mk_atom(yytext); }
        |       INOUT { $$ = mk_atom(yytext); }
        |       SHARED { $$ = mk_atom(yytext); }
                ;

basic_type:
                basic_type_x { $$ = $1; }
                ;

basic_type_x:
                BOOL { $$ = mk_atom(yytext); }
        |       BYTE { $$ = mk_atom(yytext); }
        |       UBYTE { $$ = mk_atom(yytext); }
        |       SHORT { $$ = mk_atom(yytext); }
        |       USHORT { $$ = mk_atom(yytext); }
        |       INT { $$ = mk_atom(yytext); }
        |       UINT { $$ = mk_atom(yytext); }
        |       LONG { $$ = mk_atom(yytext); }
        |       ULONG { $$ = mk_atom(yytext); }
        |       CHAR { $$ = mk_atom(yytext); }
        |       WCHAR { $$ = mk_atom(yytext); }
        |       DCHAR { $$ = mk_atom(yytext); }
        |       FLOAT { $$ = mk_atom(yytext); }
        |       DOUBLE { $$ = mk_atom(yytext); }
        |       REAL { $$ = mk_atom(yytext); }
        |       IFLOAT { $$ = mk_atom(yytext); }
        |       IDOUBLE { $$ = mk_atom(yytext); }
        |       IREAL { $$ = mk_atom(yytext); }
        |       CFLOAT { $$ = mk_atom(yytext); }
        |       CREAL { $$ = mk_atom(yytext); }
        |       VOID { $$ = mk_atom(yytext); }
                ;

basic_type2:
                basic_type2x { $$ = $1; }
        |       basic_type2x basic_type2 { $$ = ext_node($1, 1, $2); }
                ;

basic_type2x:
                '*' { $$ = mk_node("BasicType", 1, yytext); }
        |       '[' ']' {$$ = mk_none(); }
        |       '[' assign_expression ']' { $$ = mk_node("BasicType", 1, $2); }
        |       '['assign_expression DOTDOT assign_expression ']' { $$ = mk_node("BasicType", 3, $2, $3, $4); }
        |       '[' type ']' { $$ = mk_node("BasicType", 1, $2); }
        |       DELEGATE parameters { $$ = mk_node("BasicType", 2, $1, $2); }
        |       FUNCTION parameters { $$ = mk_node("BasicType", 2, $1, $2); }
                ;

identifier_list:
                IDENTIFIER { $$ = mk_node("Identifier", 1, mk_atom(yytext)); }
        |       IDENTIFIER '.' IDENTIFIER { $$ = ext_node($1, 1, $2); }
        |       template_instance { $$ = mk_node("IdentifierList", 1, mk_atom(yytext)); }
        |       template_instance '.' identifier_list { $$ = ext_node($1, 1, $2); }
        |       IDENTIFIER '[' assign_expression ']' '.' identifier_list { $$ = mk_node("IdentifierList", 3, $1, $3, $6); }
                ;

typeof:
                TYPEOF '(' expression ')' { $$ = mk_node("Typeof", 2, $1, $3); }
        |       TYPEOF '(' RETURN ')' { $$ = mk_node("Typeof", 2, $1, $3); }
                ;


/**
Expression.
*/

expression:
                comma_expression { $$ = mk_node("Expression", 1, $1); }
                ;

comma_expression:
                assign_expression { $$ = $1; }
        |       assign_expression ',' comma_expression { $$ = ext_node($1, 1, $3); }
                ;

assign_expression:
                conditional_expression { $$ = $1; }
        |       conditional_expression '=' assign_expression { $$ = mk_node("AssignExpression", 2, $1, $3); }
        |       conditional_expression PLUSEQ assign_expression { $$ = mk_node("AssignAddExpression", $1, $3); }
        |       conditional_expression MINUSEQ assign_expression { $$ = mk_node("AssignSubExpression", $1, $3); }
        |       conditional_expression STAREQ assign_expression { $$ = mk_node("AssignMulExpression", $1, $3); }
        |       conditional_expression SLASHEQ assign_expression { $$ = mk_node("AssignDivExpression", $1, $3); }
        |       conditional_expression PERCENTEQ assign_expression { $$ = mk_node("AssignRemExpression", $1, $3); }
        |       conditional_expression ANDEQ assign_expression { $$ = mk_node("AssignBitAndExpression", $1, $3); }
        |       conditional_expression OREQ assign_expression { $$ = mk_node("AssignBitOrExpression", $1, $3); }
        |       conditional_expression CARETEQ assign_expression { $$ = mk_node("AssignBitXorExpression", $1, $3); }
        |       conditional_expression TILDEEQ assign_expression { $$ = mk_node("AssignCatExpression", $1, $3); }
        |       conditional_expression LSHEQ assign_expression { $$ = mk_node("AssignShlExpression", $1, $3); }
        |       conditional_expression RSHEQ assign_expression { $$ = mk_node("AssignShrExpression", $1, $3); }
        |       conditional_expression RSHRSHEQ assign_expression { $$ = mk_node("AssignShrShrAddExpression", $1, $3); }
        |       conditional_expression CARETCARETEQ assign_expression { $$ = mk_node("AssignCaretCaretEqExpression", $1, $3); }
        ;

conditional_expression:
                oror_expression { $$ = $1; }
        |       oror_expression '?' expression ':' conditional_expression { $$ = mk_node("ConditionalExpression", 3, $1, $3, $5); }
        ;

oror_expression:
                andand_expression { $$ = $1; }
        |       oror_expression OROR andand_expression { $$ = mk_node("OrOrExpression", 2, $1, $3); }
        ;

andand_expression:
                or_expression { $$ = $1; }
        |       andand_expression ANDAND or_expression { $$ = mk_node("AndAndExpression", 2, $1, $3); }
        |       cmp_expression { $$ = $1; }
        |       andand_expression ANDAND cmp_expression { $$ = mk_node("AndAndExpression", 2, $1, $3); }
        ;

or_expression:
                xor_expression { $$ = $1; }
        |       or_expression '|' xor_expression { $$ = mk_node("OrExpression", 2, $1, $3); }
        ;

xor_expression:
                and_expression { $$ = $1; }
        |       xor_expression '^' and_expression { $$ = mk_node("XorExpression", 2, $1, $3); }
        ;

and_expression:
                shift_expression { $$ = $1; }
        |       and_expression '&' shift_expression { $$ = mk_node("AndExpression", 2, $1, $3); }
        ;

cmp_expression:
                equal_expression { $$ = $1; }
        |       identity_expression { $$ = $1; }
        |       rel_expression { $$ = $1; }
        |       in_expression { $$ = $1; }
                ;

equal_expression:
                shift_expression EQEQ shift_expression { $$ = mk_node("EqualExpression", 2, $1, $3); }
        |       shift_expression NOTEQ shift_expression { $$ = mk_node("NotEqualExpression", 2, $1, $3); }
                ;

identity_expression:
                shift_expression IS shift_expression { $$ = mk_node("IsExpression", 2, $1, $3); }
        |       shift_expression '!' IS shift_expression { $$ = mk_node("NotIsExpression", 2, $1, $3); }
                ;

rel_expression:
                shift_expression '<' shift_expression { $$ = mk_node("LessExpression", 2, $1, $3); }
        |       shift_expression LE shift_expression { $$ = mk_node("LessOrEqExpression", 2, $1, $3); }
        |       shift_expression '>' shift_expression { $$ = mk_node("GreaterExpression", 2, $1, $3); }
        |       shift_expression RE shift_expression { $$ = mk_node("GreaterOrEqExpression", 2, $1, $3); }
        |       shift_expression NOTLEREEQ shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression NOTLERE shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression LSHRSH shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression LSHRSHEQ shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression NOTRE shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression NOTREEQ shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression NOTLE shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
        |       shift_expression NOTLEEQ shift_expression { $$ = mk_node("RelExpression", 2, $1, $3); }
                ;

in_expression:
                shift_expression IN shift_expression { $$ = mk_node("InExpression", 2, $1, $3); }
        |       shift_expression '!' IN shift_expression { $$ = mk_node("NotIsExpression", 2, $1, $3); }
        ;

shift_expression:
                add_expression { $$ = $1; }
        |       shift_expression LSH add_expression { $$ = mk_node("ShiftExpression", 2, $1, $3); }
        |       shift_expression RSH add_expression { $$ = mk_node("ShiftExpression", 2, $1, $3); }
        |       shift_expression RSHRSH add_expression { $$ = mk_node("ShiftExpression", 2, $1, $3); }
        ;

add_expression:
                mul_expression { $$ = $1; }
        |       add_expression '+' mul_expression { $$ = mk_node("BinaryExpression", 3, mk_atom("BiAdd"), $1, $3); }
        |       add_expression '-' mul_expression { $$ = mk_node("BinaryExpression", 3, mk_atom("BiSub"), $1, $3); }
        |       cat_expression { $$ = $1; }
        ;

cat_expression:
                add_expression '~' mul_expression { $$ = mk_node("BinaryExpression", 3, mkd_atom("BiCat"), $1, $3); }
        ;

mul_expression:
                unary_expression { $$ = $1; }
        |       mul_expression '*' unary_expression { $$ = mk_node("BinaryExpression", 3, mk_atom("BiMul") $1, $3); }
        |       mul_expression '/' unary_expression { $$ = mk_node("BinaryExpression", 3, mk_atom("BiDiv") $1, $3); }
        |       mul_expression '%' unary_expression { $$ = mk_node("BinaryExpression", 3, mk_atom("BiRem") $1, $3); }
        ;

unary_expression:
                '&' unary_expression { $$ = mk_node("AddressOfExpression", 1, $2); }
        |       INCREMENT unary_expression { $$ = mk_node("IncrementExpression", 1, $2); }
        |       DECREMENT unary_expression { $$ = mk_node("DecrementExpression", 1, $2); }
        |       '*' unary_expression { $$ = mk_node("UnaryExpression", 2, mk_atom("Pointer"), $2); }
        |       '-' unary_expression { $$ = mk_node("UnaryExpression", 2, mk_atom("UnNeg"), $2); }
        |       '+' unary_expression { $$ = mk_node("UnaryExpression", 2, mk_atom("UnPos"), $2); }
        |       '!' unary_expression { $$ = mk_node("UnaryExpression", 2, mk_atom("UnNot"), $2); }
        |       complement_expression { $$ = $1; }
        |       delete_expression { $$ = $1; }
        |       cast_exprssion { $$ = $1; }
        |       pow_expression { $$ = $1; }
        ;

complement_expression:
                '~' unary_expression { $$ = mk_node("ComplementExpression", 1, $2); }
        ;

new_expression:
                NEW type { $$ = mk_node("NewExpression", 1, $2); }
        |       NEW allocator_arguments type { $$ = mk_node("NewExpression", 2, $2, $3); }
        |       new_expression_with_args { $$ = $1; }
        ;

new_expression_with_args:
                NEW type '[' assign_expression ']' { $$ = mk_node("NewExpression", 2, $2, $4); }
        |       NEW allocator_arguments type '[' assign_expression ']' { $$ = mk_node("NewExpression", 3, $2, $3, $5); }
        |       NEW type '(' ')' { $$ = mk_node("NewExpression", 1, $2); }
        |       NEW type allocator_arguments '(' ')' { $$ = mk_node("NewExpression", 2, $2, $3); }
        |       NEW type '(' argument_list ')' { $$ = mk_node("NewExpression", 2, $2, $4); }
        |       NEW type allocator_arguments '(' argument_list ')' { $$ = mk_node("NewExpression", 3, $2, $3, $5); }
        |       new_anon_class_expression { $$ = $1; }
        ;

allocator_arguments:
                '(' ')' { $$ = mk_none(); }
        |       '('argument_list ')' { $$ = $2; }
        ;

argument_list:
                assign_expression { $$ = $1; }
        |       assign_expression ',' { $$ = $1; }
        |       assign_expression ',' argument_list { $$ = ext_node($1, 1, $3); }
        ;

new_anon_class_expression:
                NEW CLASS aggregate_body { $$ = mk_node("AnonClassExpression", 1, $3); }
        |       NEW allocator_arguments CLASS aggregate_body { $$ = mk_node("AnonClassExpression", 2, $2, $4); }
        |       NEW CLASS class_arguments aggregate_body { $$ = mk_node("AnonClassExpression", 2, $3, $4); }
        |       NEW CLASS super_class aggregate_body { $$ = mk_node("AnonClassExpression", 2, $3, $4); }
        |       NEW CLASS interfaces aggregate_body { $$ = mk_node("AnonClassExpression", 2, $3, $4); }
        |       NEW allocator_arguments CLASS class_arguments aggregate_body
        |       NEW allocator_arguments CLASS super_class aggregate_body
        |       NEW allocator_arguments CLASS interfaces aggregate_body
        |       NEW CLASS class_arguments super_class aggregate_body
        |       NEW CLASS class_arguments interfaces aggregate_body
        |       NEW CLASS super_class interfaces aggregate_body
        |       NEW allocator_arguments CLASS class_arguments super_class aggregate_body
        |       NEW allocator_arguments CLASS class_arguments interfaces aggregate_body
        |       NEW allocator_arguments CLASS super_class interfaces aggregate_body
        |       NEW CLASS class_arguments super_class interfaces aggregate_body
        |       NEW allocator_arguments CLASS class_arguments super_class interfaces aggregate_body
                ;

class_arguments:
                '(' ')' { $$ = mk_none; }
        |       '('argument_list ')' { $$ = mk_node("ClassArgument", 1, $2); }
        ;

delete_expression:
                DELETE unary_expression { $$ = mk_node("DeleteExpression", 1, $2); }
        ;

cast_exprssion:
                CAST '(' type ')' unary_expression { $$ = mk_node("CastExpression", 2, $3, $5); }
        |       CAST '(' ')' unary_expression { $$ = mk_node("CastExpression", 1, $4); }
        |       CAST '(' type_ctors ')' unary_expression { $$ = mk_node("CastExpression", 2, $3, $5); }
        ;

pow_expression:
                postfix_expression { $$ = $1; }
        |       postfix_expression CARETCARET postfix_expression { $$ = mk_node("PowExpression", 1, $3); }
        ;

postfix_expression:
                primary_expression { $$ = $1; }
        |       postfix_expression '.' IDENTIFIER { $$ = mk_node("PostfixExpression", 1, $3); }
        |       postfix_expression '.' template_instance { $$ = mk_node("PostfixExpression", 1, $3); }
        |       postfix_expression '.' new_expression { $$ = mk_node("PostfixExpression", 1, $3); }
        |       postfix_expression INCREMENT { $$ = mk_node("Increment", 1, $2); }
        |       postfix_expression DECREMENT { $$ = mk_node("Decrement", 1, $2); }
        |       postfix_expression '(' ')' { $$ = $1; }
        |       postfix_expression '(' argument_list ')' { $$ = mk_node("PostfixExpression", 2, $1, $3); }
        |       basic_type '(' ')' { $$ = mk_node("PostfixExpression", 1, $1); }
        |       type_ctors basic_type '(' ')' { $$ = mk_node("PostfixExpression", 2, $1, $2); }
        |       basic_type '(' argument_list ')' { $$ = mk_node("PostfixExpression", 2, $1, $3); }
        |       type_ctors '(' argument_list ')' { $$ = mk_node("PostfixExpression", 2, $1, $3); }
        |       index_expression { $$ = $1; }
        |       slice_expression { $$ = $1; }
        ;

index_expression:
                postfix_expression '[' argument_list ']' { $$ = mk_node("IndexExpression", 2, $1, %3); }
        ;

slice_expression:
                postfix_expression '[' ']' { $$ = mk_node("SliceExpression", 1, $1); }
        |       postfix_expression '['slice ']' { $$ = mk_node("SliceExpression", 2, $1, $3); }
        |       postfix_expression '['slice ',' ']' { $$ = mk_node("SliceExpression", 2, $1, $3); }
        ;

slice:
                assign_expression { $$ = $1; }
        |       assign_expression ',' slice { $$ = mk_node("Slice", 2, $1, $3); }
        |       assign_expression DOTDOT assign_expression { $$ = mk_node("Slice", 2, $1, $3); }
        |       assign_expression DOTDOT assign_expression ',' slice { $$ = mk_node("Slice", 3, $1, $3, $5); }
        ;

primary_expression:
                IDENTIFIER { $$ = mk_atom(yytext); }
        |       '.'IDENTIFIER { $$ = mk_atom(yytext); }
        |       THIS { $$ = mk_atom(yytext); }
        |       SUPER { $$ = mk_atom(yytext); }
        |       NULLKEYWORD { $$ = mk_atom(yytext); }
        |       TRUE { $$ = mk_atom(yytext); }
        |       FALSE { $$ = mk_atom(yytext); }
        |       '$'  { $$ = mk_atom(yytext); }
        |       INTEGER { $$ = mk_atom(yytext); }
        |       FLOATINGPOINT { $$ = mk_atom(yytext); }
        |       string_literals { $$ = $1; }
        |       array_literal { $$ = $1; }
        |       assoc_array_literal { $$ = $1; }
        |       assert_expression { $$ = $1; }
        |       mixin_expression { $$ = $1; }
        |       import_expression { $$ = $1; }
        |       new_expression_with_args { $$ = $1; }
        |       basic_type_x '.' IDENTIFIER { $$ = mk_node("PrimaryExpression", 2, $1, $3); }
        |       typeof { $$ = $1; }
        |       typeid_expression { $$ = $1; }
        |       is_expression { $$ = $1; }
        |       '(' expression ')' { $$ = $2; }
        |       traits_expression { $$ = $1; }
        |       special_keyword { $$ = $1; }
        ;

string_literals:
                string_literal { $$ = $1; }
        |       string_literal string_literals { $$ = ext_node($1, 1, $2); }
        ;

string_literal:
                DOUBLEQOUTEDSTRING { $$ = mk_atom(yytext); }
        |       WYSIWYGSTRING { $$ = mk_atom(yytext); }
        |       TOKENSTRING { $$ = mk_atom(yytext); }
        ;

array_literal:
                '[' ']' { $$ = mk_node("ArrayLiteral", 2, $1, $2); }
        |       '['argument_list ']' { $$ = mk_node("ArrayLiteral", 3, $1, $2, $3); }
        ;

assoc_array_literal:
                '[' key_value_pairs ']' { $$ = mk_node("AssocArrayLiteral", 3, $1, $2, $3); }
        ;

key_value_pairs:
                key_value_pair { $$ = $1; }
        |       key_value_pair ',' key_value_pairs { $$ = ext_node($1, 1, $3); }
        ;

key_value_pair:
                key_expression ':' value_expression { $$ = mk_node("KeyValuePair", 2, $1, $3); }
        ;

key_expression:
                assign_expression { $$ = $1; }
        ;

value_expression:
                assign_expression { $$ = $1; }
        ;

function_literal:
                FUNCTION function_literal_body { $$ = mk_node("FunctionLiteral", 1, $2); }
        |       FUNCTION type function_literal_body { $$ = mk_node("FunctionLiteral", 2, $2, $3); }
        |       FUNCTION parameter_attributes function_literal_body { $$ = mk_node("FunctionLiteral", 2, $2, $3); }
        |       FUNCTION type parameter_attributes function_literal_body { $$ = mk_node("FunctionLiteral", 3, $2, $3, $4); }
        |       DELEGATE function_literal_body { $$ = mk_node("FunctionLiteral", 1, $2); }
        |       DELEGATE type function_literal_body { $$ = mk_node("FunctionLiteral", 2, $2, $3); }
        |       DELEGATE parameter_member_attributes function_literal_body { $$ = mk_node("FunctionLiteral", 2, $2, $3); }
        |       DELEGATE type parameter_member_attributes function_literal_body { $$ = mk_node("FunctionLiteral", 3, $2, $3, $4); }
        |       parameter_member_attributes function_literal_body { $$ = mk_node("FunctionLiteral", 2, $1, $2); }
        |       function_literal_body { $$ = $1; }
        |       lambda { $$ = $1; }
        ;

parameter_attributes:
                parameters { $$ = $1; }
        |       parameters function_attributes { $$ = mk_node("ParameterAttributes", 2, $1, $2); }
        ;

parameter_member_attributes:
                parameters { $$ = $1; }
        |       parameters member_function_attributes { $$ = mk_node("ParameterMemberAttributes", 2, $1, $2); }
        ;

function_literal_body:
                block_statement { $$ = $1; }
        |       body_statement { $$ = $1; }
        |       function_contracts body_statement { $$ = mk_node("FunctionLiteralBody", 2, $1, $2); }
        ;

lambda:
                FUNCTION parameter_attributes EQRSH assign_expression { $$ = mk_node("Lambda", 2, $2, $4); }
        |       FUNCTION type parameter_attributes EQRSH assign_expression { $$ = mk_node("Lambda", 3, $2, $3, $5); }
        |       DELEGATE parameter_member_attributes EQRSH assign_expression { $$ = mk_node("Lambda", 2, $2, $4); }
        |       DELEGATE type parameter_member_attributes EQRSH assign_expression { $$ = mk_node("Lambda", 3, $2, $3, $5); }
        |       parameter_member_attributes EQRSH assign_expression { $$ = mk_node("Lambda", 2, $1, $3); }
        |       IDENTIFIER EQRSH assign_expression { $$ = mk_node("Lambda", 2, $1, $3); }
        ;

assert_expression:
                ASSERT '(' assign_expression ')' { $$ = mk_node("AssertExpression", 1, $3); }
        |       ASSERT '(' assign_expression ',' ')' { $$ = mk_node("AssertExpression", 1, $3); }
        |       ASSERT '(' assign_expression ',' assign_expression ')' { $$ = mk_node("AssertExpression", 2, $3, $5); }
        |       ASSERT '(' assign_expression ',' assign_expression ',' ')' { $$ = mk_node("AssertExpression", 2, $3, $5); }
        ;

mixin_expression:
                MIXIN '(' assign_expression ')' { $$ = mk_node("MixinExpression", 1, $3); }
        ;

import_expression:
                IMPORT '(' assign_expression ')' { $$ = mk_node("ImportExpression", 1, $3); }
        ;

typeid_expression:
                TYPEID '(' type ')' { $$ = mk_node("TypeidExpression", 1, $3); }
        |       TYPEID '(' expression ')' { $$ = mk_node("TypeidExpression", 1, $3); }
                ;

is_expression:
                IS '(' type ')' { $$ = mk_node("IsExpression", 1, $3); }
        |       IS '(' type ':' type_specialization ')' { $$ = mk_node("IsExpression", 2, $3, $5); }
        |       IS '(' type EQEQ type_specialization ')' { $$ = mk_node("IsExpression", 2, $3, $5); }
        |       IS '(' type ':' type_specialization ',' template_parameter_list ')' { $$ = mk_node("IsExpression", 3, $3, $5, $7); }
        |       IS '(' type EQEQ type_specialization ',' template_parameter_list ')' { $$ = mk_node("IsExpression", 3, $3, $5, $7); }
        |       IS '(' type IDENTIFIER ')' { $$ = mk_node("IsExpression", 2, $3, $4); }
        |       IS '(' type IDENTIFIER ':' type_specialization ')' { $$ = mk_node("IsExpression", 3, $3, $4, $6); }
        |       IS '(' type IDENTIFIER EQEQ type_specialization ')' { $$ = mk_node("IsExpression", 3, $3, $4, $6); }
        |       IS '(' type IDENTIFIER ':' type_specialization ',' template_parameter_list ')' { $$ = mk_node("IsExpression", 4, $3, $4, $6, $8); }
        |       IS '(' type IDENTIFIER EQEQ type_specialization ',' template_parameter_list ')' { $$ = mk_node("IsExpression", 4, $3, $4, $6, $8); }
                ;

type_specialization:
                type { $$ = $1; }
        |       STRUCT { $$ = mk_atom(yytext); }
        |       UNION { $$ = mk_atom(yytext); }
        |       CLASS { $$ = mk_atom(yytext); }
        |       INTERFACE { $$ = mk_atom(yytext); }
        |       ENUM { $$ = mk_atom(yytext); }
        |       FUNCTION { $$ = mk_atom(yytext); }
        |       DELEGATE { $$ = mk_atom(yytext); }
        |       SUPER { $$ = mk_atom(yytext); }
        |       CONST { $$ = mk_atom(yytext); }
        |       IMMUTABLE { $$ = mk_atom(yytext); }
        |       INOUT { $$ = mk_atom(yytext); }
        |       SHARED { $$ = mk_atom(yytext); }
        |       RETURN { $$ = mk_atom(yytext); }
        |       SPECIAL_PARAMETERS { $$ = mk_atom(yytext); }
                ;

traits_expression:
                SPECIAL_TRAITS '(' traits_keyword ',' traits_arguments ')' { $$ = mk_node("TraitsExpression", 2, $3, $5); }
                ;

traits_keyword:
                TRAITS_ISABSTRACTCLASS { $$ = mk_atom(yytext); }
        |       TRAITS_ISARITHMETIC { $$ = mk_atom(yytext); }
        |       TRAITS_ISASSOCIATIVEARRAY { $$ = mk_atom(yytext); }
        |       TRAITS_ISFINALCLASS { $$ = mk_atom(yytext); }
        |       TRAITS_ISPOD { $$ = mk_atom(yytext); }
        |       TRAITS_ISNESTED { $$ = mk_atom(yytext); }
        |       TRAITS_ISFLOATING { $$ = mk_atom(yytext); }
        |       TRAITS_INTEGRAL { $$ = mk_atom(yytext); }
        |       TRAITS_ISSCALAR { $$ = mk_atom(yytext); }
        |       TRAITS_ISSTATICARRAY { $$ = mk_atom(yytext); }
        |       TRAITS_ISUNSIGNED { $$ = mk_atom(yytext); }
        |       TRAITS_ISVIRTUALFUNCTION { $$ = mk_atom(yytext); }
        |       TRAITS_ISVIRTUALMETHOD { $$ = mk_atom(yytext); }
        |       TRAITS_ISABSTRACTFUNCTION { $$ = mk_atom(yytext); }
        |       TRAITS_ISFINALFUNCTION { $$ = mk_atom(yytext); }
        |       TRAITS_ISSTATICFUNCTION { $$ = mk_atom(yytext); }
        |       TRAITS_ISOVERRIDEFUNCTION { $$ = mk_atom(yytext); }
        |       TRAITS_ISTEMPLATE { $$ = mk_atom(yytext); }
        |       TRAITS_ISREF { $$ = mk_atom(yytext); }
        |       TRAITS_ISOUT { $$ = mk_atom(yytext); }
        |       TRAITS_ISLAZY { $$ = mk_atom(yytext); }
        |       TRAITS_HASMANY { $$ = mk_atom(yytext); }
        |       TRAITS_IDENTIFIER { $$ = mk_atom(yytext); }
        |       TRAITS_GETALIASTHIS { $$ = mk_atom(yytext); }
        |       TRAITS_GETATTRIBUTES { $$ = mk_atom(yytext); }
        |       TRAITS_GETFUNCTIONATTRIBUTES { $$ = mk_atom(yytext); }
        |       TRAITS_GETFUNCTIONVARIADICSTYLE { $$ = mk_atom(yytext); }
        |       TRAITS_GETLINKAGE { $$ = mk_atom(yytext); }
        |       TRAITS_GETMEMBER { $$ = mk_atom(yytext); }
        |       TRAITS_GETOVERLOADS { $$ = mk_atom(yytext); }
        |       TRAITS_GETPARAMETERSTORAGECLASSES { $$ = mk_atom(yytext); }
        |       TRAITS_GETPOINTERBITMAP { $$ = mk_atom(yytext); }
        |       TRAITS_GETPROTECTION { $$ = mk_atom(yytext); }
        |       TRAITS_GETVIRTUALFUNCTIONS { $$ = mk_atom(yytext); }
        |       TRAITS_GETVIRTUALMETHODS { $$ = mk_atom(yytext); }
        |       TRAITS_UNITTEST { $$ = mk_atom(yytext); }
        |       TRAITS_PARENT { $$ = mk_atom(yytext); }
        |       TRAITS_CLASSINSTANCESIZE { $$ = mk_atom(yytext); }
        |       TRAITS_GETVIRTUALINDEX { $$ = mk_atom(yytext); }
        |       TRAITS_ALLMEMBERS { $$ = mk_atom(yytext); }
        |       TRAITS_DERIVEDMEMBERS { $$ = mk_atom(yytext); }
        |       TRAITS_ISSAME { $$ = mk_atom(yytext); }
        |       TRAITS_COMPILES { $$ = mk_atom(yytext); }
                ;

traits_arguments:
                traits_argument { $$ = $1; }
        |       traits_argument ',' traits_arguments { $$ = mk_node("TraitsArgument", 2, $1, $3); }
                ;

traits_argument:
                assign_expression { $$ = $1; }
        |       type { $$ = $1; }
                ;

special_keyword:
                SPECIAL_FILE { $$ = mk_atom(yytext); }
        |       SPECIAL_FILEFULLPATH { $$ = mk_atom(yytext); }
        |       SPECIAL_MODULE { $$ = mk_atom(yytext); }
        |       SPECIAL_LINE { $$ = mk_atom(yytext); }
        |       SPECIAL_FUNCTION { $$ = mk_atom(yytext); }
        |       SPECIAL_PRETTYFUNCION { $$ = mk_atom(yytext); }
                ;

/**
Statement.
*/

statement:
                ';' { $$ = mk_none(); }
        |       non_empty_statement { $$ = mk_node("Statement", 1, $1); }
        |       scope_block_statement { $$ = mk_node("Statement", 1, $1); }
        ;

no_scope_non_empty_statement:
                non_empty_statement { $$ = $1; }
        |       block_statement { $$ = $1; }
        ;

no_scope_statement:
                ';' { $$ = mk_none(); }
        |       non_empty_statement { $$ = $1; }
        |       block_statement { $$ = $1; }
        ;

non_empty_statement:
                non_empty_statement_no_case_no_default { $$ = $1; }
        |       case_statement { $$ = $1; }
        |       case_range_statement { $$ = $1; }
        |       default_statement { $$ = $1; }
        ;

non_empty_statement_no_case_no_default:
                labeled_statement { $$ = $1; }
        |       expression_statement { $$ = $1; }
        |       declaration_statement { $$ = $1; }
        |       if_statement { $$ = $1; }
        |       while_statement { $$ = $1; }
        |       do_statement { $$ = $1; }
        |       for_statement { $$ = $1; }
        |       foreach_statement { $$ = $1; }
        |       switch_statement { $$ = $1; }
        |       final_switch_statement { $$ = $1; }
        |       continue_statement { $$ = $1; }
        |       break_statement { $$ = $1; }
        |       return_statement { $$ = $1; }
        |       goto_statement { $$ = $1; }
        |       with_statement { $$ = $1; }
        |       synchronized_statement { $$ = $1; }
        |       try_statement { $$ = $1; }
        |       scope_block_statement { $$ = $1; }
        |       throw_statement { $$ = $1; }
        /* |       asm_statement */
        |       pragma_statement { $$ = $1; }
        |       mixin_statement { $$ = $1; }
        |       foreach_range_statement { $$ = $1; }
        |       conditional_statement { $$ = $1; }
        |       static_foreach_statement { $$ = $1; }
        |       static_assert { $$ = $1; }
        |       template_mixin { $$ = $1; }
        |       import_declaration { $$ = $1; }
                ;

scope_statement:
                non_empty_statement { $$ = $1; }
                ;

scope_block_statement:
                block_statement { $$ = $1; }
                ;

labeled_statement:
                IDENTIFIER ':' { $$ = mk_node("LabeledStatement", 1, $1); }
        |       IDENTIFIER ':' no_scope_statement { $$ = mk_node("LabeledStatement", 2, $1, $3); }
        |       IDENTIFIER ':' statement { $$ = mk_node("LabeledStatement", 2, $1, $3); }
                ;

block_statement:
                '{' '}' { $$ = mk_none(); }
        |       '{'statement_list '}' { $$ = $2; }
                ;

statement_list:
                statement { $$ = $1; }
        |       statement statement_list { $$ = ext_node($1, 1, $2); }
                ;

expression_statement:
                expression ';' { $$ = $1; }
                ;

declaration_statement:
                declaration { $$ = mk_node("DeclarationStatement", 1, $1); }
        |       storage_classes declaration { $$ = mk_node("DeclarationStatement", 2, $1, $2); }
                ;

if_statement:
                IF '(' if_condition ')' then_statement { $$ = mk_node("IfStatement", 2, $3, $5); }
        |       IF '(' if_condition ')' then_statement ELSE else_statement { $$ = mk_node("IfStatement", 3, $3, $5, $7); }
                ;

if_condition:
                expression { $$ = mk_node("IfCondition", 1, $1); }
        |       AUTO IDENTIFIER '=' expression { $$ = mk_node("IfCondition", 3, $1, $2, $4); }
        |       type_ctors IDENTIFIER '=' expression { $$ = mk_node("IfCondition", 3, $1, $2, $4); }
        |       basic_type declaration '=' expression { $$ = mk_node("IfCondition", 3, $1, $2, $4); }
        |       type_ctors basic_type declaration '=' expression { $$ = mk_node("IfCondition", 4, $1, $2, $3, $5); }
                ;

then_statement:
                scope_statement { $$ = $1; }
                ;

else_statement:
                scope_statement { $$ = $1; }
                ;

while_statement:
                WHILE '(' expression ')' scope_statement { $$ = mk_node("WhileStatement", 2, $3, $5); }
                ;

do_statement:
                DO scope_statement WHILE '(' expression ')' { $$ = mk_node("DoStatement", 2, $2, $5); }
                ;

for_statement:
                FOR '(' initialize ';' ')' scope_statement { $$ = mk_node("ForStatement", 2, $3, $6); }
        |       FOR '(' initialize ';' increment ')' scope_statement { $$ = mk_node("ForStatement", 3, $3, $5, $7); }
                ;

initialize:
               ';' { $$ = mk_none(); }
        |       no_scope_non_empty_statement { $$ = $1; }
                ;

increment:
                expression { $$ = $1; }
                ;

aggregate_foreach:
                foreach '(' foreach_type_list ';' foreach_aggregate ')' { $$ = mk_node("AggregateForeach", 3, $1, $3, $5); }
                ;

foreach_statement:
                aggregate_foreach no_scope_non_empty_statement { $$ = mk_node("ForeachStatement", 2, $1, $2); }
                ;

foreach:
                FOREACH { $$ = mk_atom(yytext); }
        |       FOREACHREVERSE { $$ = mk_atom(yytext); }
                ;

foreach_type_list:
                foreach_type { $$ = $1; }
        |       foreach_type ',' foreach_type_list { $$ = ext_node($1, 1, $3); }
                ;

foreach_type:
                basic_type declaration { $$ = mk_node("ForeachType" 2, $1, $2); }
        |       foreach_type_attribute basic_type declaration { $$ = mk_node("ForeachType" 3, $1, $2, $3); }
        |       IDENTIFIER { $$ = mk_node("ForeachType", 1, $1); }
        |       foreach_type_attribute IDENTIFIER { $$ = mk_node("ForeachType" 2, $1, $2); }
        |       ALIAS IDENTIFIER { $$ = mk_node("ForeachType" 2, $1, $2); }
        |       foreach_type_attribute ALIAS IDENTIFIER { $$ = mk_node("ForeachType" 3, $1, $2, $3); }
                ;

foreach_type_attribute:
                REF { $$ = mk_atom(yytext); }
        |       type_ctor { $$ = $1; }
        |       ENUM { $$ = mk_atom(yytext); }
                ;

foreach_aggregate:
                expression { $$ = $1; }
                ;

range_foreach:
                foreach '(' foreach_type ';' lwr_expression DOTDOT upr_expression ')' { $$ = mk_node("RangeForeach", 4, $1, $3, $5, $7); }
                ;

lwr_expression:
                expression { $$ = $1; }
                ;

upr_expression:
                expression { $$ = $1; }
                ;

foreach_range_statement:
                range_foreach scope_statement { $$ = mk_node("ForeachRangeStatement", 2, $1, $2); }
                ;

switch_statement:
                SWITCH '(' expression ')' scope_statement { $$ = mk_node("SwitchStatement", 2, $3, $5); }
                ;

case_statement:
                CASE argument_list ':' scope_statement { $$ = mk_node("CaseStatement", 2, $2, $4); }
                ;

case_range_statement:
                CASE first_exp ':' DOTDOT CASE last_exp ':' scope_statement { $$ = mk_node("CaseRangeStatement", 3, $2, $6, $8); }
                ;

first_exp:
                assign_expression { $$ = $1; }
                ;

last_exp:
                assign_expression { $$ = $1; }
                ;

default_statement:
                DEFAULT ':' scope_statement_list { $$ = mk_node("DefaultStatement", 1, $3); }
                ;

scope_statement_list:
                statement_list_no_case_no_default { $$ = $1; }
                ;

statement_list_no_case_no_default:
                statement_no_case_no_default { $$ = $1; }
        |       statement_no_case_no_default statement_list_no_case_no_default { $$ = ext_node($1, 1, $2); }
                ;

statement_no_case_no_default:
                ';' { $$ = mk_none(); }
        |       non_empty_statement_no_case_no_default { $$ = $1; }
                ;

final_switch_statement:
                FINAL SWITCH '(' expression ')' scope_statement { $$ = mk_node("FinalSwitchStatement", 2, $4, $6); }
                ;

continue_statement:
                CONTINUE ';' { $$ = mk_node("ContinueStatement", 1, mk_none()); }
        |       CONTINUE IDENTIFIER ';' { $$ = mk_node("ContinueStatement", 1, $2); }
                ;

break_statement:
                BREAK ';' { $$ = mk_node("BreakStatement", 1, mk_none()); }
        |       BREAK IDENTIFIER ';' { $$ = mk_node("BreakStatement", 1, $2); }
                ;

return_statement:
                RETURN ';' { $$ = mk_node("ReturnStatement", 1, mk_none()); }
        |       RETURN expression ';' { $$ = mk_node("ReturnStatement", 1, $2); }
                ;

goto_statement:
                GOTO IDENTIFIER ';' { $$ = mk_node("GotoStatement", 1, $2); }
        |       GOTO DEFAULT ';' { $$ = mk_node("GotoStatement", 1, mk_atom("default")); }
        |       GOTO CASE ';' { $$ = mk_node("GotoStatement", 1, mk_atom("case")); }
        |       GOTO CASE expression ';' { $$ = mk_node("GotoStatement", 2, mk_atom("case"), $3); }
                ;

with_statement:
                WITH '(' expression ')' scope_statement { $$ = mk_node("WithStatement", 2, $3, $5); }
        |       WITH '(' symbol ')' scope_statement { $$ = mk_node("WithStatement", 2, $3, $5); }
        |       WITH '(' template_instance ')' scope_statement { $$ = mk_node("WithStatement", 2, $3, $5); }
                ;

synchronized_statement:
                SYNCHRONIZED scope_statement { $$ = mk_node("SynchronizedStatement", 1, $2); }
        |       SYNCHRONIZED '(' expression ')' scope_statement { $$ = mk_node("SynchronizedStatement", 2, $3, $5); }
                ;

try_statement:
                TRY scope_statement catches { $$ = mk_node("TryStatement", 2, $2, $3); }
        |       TRY scope_statement catches finally_statement { $$ = mk_node("TryStatement", 3, $2, $3, $4); }
        |       TRY scope_statement finally_statement { $$ = mk_node("TryStatement", 2, $2, $3); }
                ;

catches:
                catch { $$ = $1; }
        |       catch catches { $$ = ext_node($1, 1, $2); }
                ;

catch:
                CATCH '(' catch_parameter ')' no_scope_non_empty_statement { $$ = mk_node("Catch", 2, $3, $5); }
                ;

catch_parameter:
                basic_type IDENTIFIER { $$ = mk_node("CatchParameter", 2, $1, $2); }
                ;

finally_statement:
                FINALLY no_scope_non_empty_statement { $$ = mk_node("FinallyStatement", 1, $2); }
                ;

throw_statement:
                THROW expression { $$ = mk_node("ThrowStatement", 1, $2); }
                ;

pragma_statement:
                pragma no_scope_statement { $$ = mk_node("PragmaStatement", 2, $1, $2); }
                ;

mixin_statement:
                MIXIN '(' assign_expression ')' { $$ = mk_node("MixinStatement", 1, $3); }
                ;

/**
Iasm.
*/

/**
Declaration.
*/

declaration:
                func_declaration { $$ = $1; }
        |       var_declarations { $$ = $1; }
        |       alias_declaration { $$ = $1; }
        |       aggregate_declaration { $$ = $1; }
        |       enum_declaration { $$ = $1; }
        |       import_declaration { $$ = $1; }
        |       conditional_declaration { $$ = $1; }
        |       static_foreach_declaration { $$ = $1; }
        |       static_assert { $$ = $1; }
        ;

alias_declaration:
                ALIAS basic_type declarator ';' { $$ = mk_node("AliasDeclaration", 2, $2, $3); }
        |       ALIAS storage_classes basic_type declarator ';' { $$ = mk_node("AliasDeclaration", 3, $2, $3, $4); }
        |       ALIAS basic_type func_declarator ';' { $$ = mk_node("AliasDeclaration", 2, $2, $3); }
        |       ALIAS storage_classes basic_type func_declarator ';' { $$ = mk_node("AliasDeclaration", 3, $2, $3, $4); }
        |       ALIAS alias_declaration_x ';' { $$ = mk_node("AliasDeclaration", 1, $2); }
        ;

alias_declaration_x:
                alias_declaration_y { $$ = $1; }
        |       alias_declaration_x ',' alias_declaration_y { $$ = ext_node($1, 1, $3); }
        ;

alias_declaration_y:
                IDENTIFIER '=' type { $$ = mk_node("AliasDeclarationY", 2, $1, $3); }
        |       IDENTIFIER template_parameters '=' type { $$ = mk_node("AliasDeclarationY", 3, $1, $2, $4); }
        |       IDENTIFIER '=' storage_classes type { $$ = mk_node("AliasDeclarationY", 3, $1, $3, $4); }
        |       IDENTIFIER template_parameters '=' storage_classes type { $$ = mk_node("AliasDeclarationY", 4, $1, $2, $4, $5); }
        |       IDENTIFIER '=' function_literal { $$ = mk_node("AliasDeclarationY", 2, $1, $3); }
        |       IDENTIFIER template_parameters '=' function_literal { $$ = mk_node("AliasDeclarationY", 3, $1, $2, $4); }
        ;

auto_declaration:
                storage_classes auto_declaration_x ';' { $$ = mk_node("AutoDeclaration", 2, $1, $2); }
        ;

auto_declaration_x:
                auto_declaration_y { $$ = $1; }
        |       auto_declaration_x ',' auto_declaration_y { $$ = ext_node($1, 1, $3); }
        ;

auto_declaration_y:
                IDENTIFIER '=' initializer { $$ = mk_node("AutoDeclarationY", 2, $1, $3); }
        |       IDENTIFIER template_parameters '=' initializer { $$ = mk_node("AutoDeclarationY", 3, $1, $2, $4); }
        ;

var_declarations:
                storage_classes basic_type declarators ';' { $$ = mk_node("VarDeclaration", 3, $1, $2, $3); }
        |       auto_declaration { $$ = $1; }
        ;

declarators:
                declarator_initializer { $$ = $1; }
        |       declarator_initializer ',' declarator_identifier_list { $$ = ext_node($1, 1, $3); }
        ;

declarator_initializer:
                var_declarator { $$ = mk_node("DeclarationInitializer", 1, $1); }
        |       var_declarator '=' initializer { $$ = mk_node("DeclarationInitializer", 2, $1, $3); }
        |       var_declarator template_parameters '=' initializer { $$ = mk_node("DeclarationInitializer", 3, $1, $2, $4); }
        |       alt_declarator { $$ = $1; }
        |       alt_declarator '=' initializer { $$ = mk_node("DeclarationInitializer", 2, $1, $3); }
        ;

declarator_identifier_list:
                declarator_identifier { $$ = $1; }
        |       declarator_identifier ',' declarator_identifier_list { $$ = ext_node($1, 1, $3); }
        ;

declarator_identifier:
                var_declarator_identifier { $$ = $1; }
        |       alt_declarator_identifier { $$ = $1; }
        ;

var_declarator_identifier:
                IDENTIFIER
        |       IDENTIFIER '=' initializer
        |       IDENTIFIER template_parameters '=' initializer
        ;

alt_declarator_identifier:
                basic_type2 IDENTIFIER
        |       basic_type2 IDENTIFIER alt_declarator_suffixes
        |       basic_type2 IDENTIFIER '=' initializer
        |       basic_type2 IDENTIFIER alt_declarator_suffixes '=' initializer
        |       IDENTIFIER alt_declarator_suffixes
        |       IDENTIFIER alt_declarator_suffixes '=' initializer
        ;

declarator:
                var_declarator { $$ = $1; }
        |       alt_declarator { $$ = $1; }
        ;

var_declarator:
                IDENTIFIER { $$ = mk_node("VarDeclarator", 1, $1); }
        |       basic_type2 IDENTIFIER { $$ = mk_node("VarDeclarator", 2, $1, $2); }
        ;

alt_declarator:
                IDENTIFIER alt_declarator_suffixes
        |       basic_type2 IDENTIFIER alt_declarator_suffixes
        |       '(' alt_declarator_x ')'
        |       basic_type2 '(' alt_declarator_x ')'
        |       '(' alt_declarator_x ')' alt_func_declarator_suffix
        |       basic_type2 '(' alt_declarator_x ')' alt_func_declarator_suffix
        |       '(' alt_declarator_x ')' alt_declarator_suffixes
        |       basic_type2 '(' alt_declarator_x ')' alt_declarator_suffixes
        ;

alt_declarator_x:
                IDENTIFIER
        |       basic_type2 IDENTIFIER
        |       IDENTIFIER alt_func_declarator_suffix
        |       basic_type2 IDENTIFIER alt_func_declarator_suffix
        |       alt_declarator { $$ = $1; }
        ;

alt_declarator_suffixes:
                alt_declarator_suffix { $$ = $1; }
        |       alt_declarator_suffix alt_declarator_suffixes { $$ = ext_node($1, 1, $2); }
        ;

alt_declarator_suffix:
                '[' ']' { $$ = mk_none(); }
        |       '[' assign_expression ']' { $$ = $2; }
        |       '[' type ']' { $$ = $2; }
        ;

alt_func_declarator_suffix:
                parameters { $$ = $1; }
        |       parameters member_function_attributes { $$ = ext_node($1, 1, $2); }
        ;

storage_classes:
                storage_class { $$ = $1; }
        |       storage_class storage_classes { $$ = ext_node($1, 1, $2); }
        ;

storage_class:
                DEPRECATED { $$ = mk_atom(yytext); }
        |       ENUM { $$ = mk_atom(yytext); }
        |       STATIC { $$ = mk_atom(yytext); }
        |       EXTERN { $$ = mk_atom(yytext); }
        |       ABSTRACT { $$ = mk_atom(yytext); }
        |       FINAL { $$ = mk_atom(yytext); }
        |       OVERRIDE { $$ = mk_atom(yytext); }
        |       SYNCHRONIZED { $$ = mk_atom(yytext); }
        |       AUTO { $$ = mk_atom(yytext); }
        |       SCOPE { $$ = mk_atom(yytext); }
        |       CONST { $$ = mk_atom(yytext); }
        |       IMMUTABLE { $$ = mk_atom(yytext); }
        |       INOUT { $$ = mk_atom(yytext); }
        |       SHARED { $$ = mk_atom(yytext); }
        |       SPECIAL_GSHARED { $$ = mk_atom(yytext); }
        |       property { $$ = $1; }
        |       NOTHROW { $$ = mk_atom(yytext); }
        |       PURE { $$ = mk_atom(yytext); }
        |       REF { $$ = mk_atom(yytext); }
        ;

initializer:
                void_initializer { $$ = $1; }
        |       non_void_initializer { $$ = $1; }
                ;

void_initializer:
                VOID { $$ = mk_atom(yytext); }
                ;

non_void_initializer:
                exp_initializer { $$ = $1; }
        |       struct_initializer { $$ = $1; }
                ;

exp_initializer:
                assign_expression { $$ = $1; }
                ;

struct_initializer:
                '{' '}' { $$ = mk_none(); }
        |       '{' struct_member_initializers '}' { $$ = $2; }
                ;

struct_member_initializers:
                struct_member_initializer { $$ = $1; }
        |       struct_member_initializer ',' { $$ = $1; }
        |       struct_member_initializer ',' struct_member_initializers { $$ = ext_node($1, 1, $3); }
                ;

struct_member_initializer:
                non_void_initializer { $$ = mk_node("StructMemberInitializer", 1, $1); }
        |       IDENTIFIER ':' non_void_initializer  { $$ = mk_node("StructMemberInitializer", 2, $1, $3); }
                ;

/**
Function.
*/

func_declaration:
                basic_type func_declarator function_body { $$ = mk_node("FuncDeclaration", 3, $1, $2, $3); }
        |       storage_classes basic_type func_declarator function_body { $$ = mk_node("FuncDeclaration", 4, $1, $2, $3, $4); }
        |       auto_func_declaration { $$ = $1; }
                ;

auto_func_declaration:
                storage_classes IDENTIFIER func_declarator_suffix function_body { $$ = mk_node("FuncDeclaration", 4, $1, $2, $3, $4); }
                ;

func_declarator:
                IDENTIFIER func_declarator_suffix { $$ = mk_node("FuncDeclarator", 2, $1, $2); }
                basic_type2 IDENTIFIER func_declarator_suffix { $$ = mk_node("FuncDeclarator", 3, $1, $2, $3); }
                ;

func_declarator_suffix:
                parameters { $$ = $1; }
        |       parameters member_function_attributes { $$ = ext_node($1, 1, $2); }
                ;

parameters:
                '(' ')' { $$ = mk_none(); }
        |       '(' parameter_list ')' { $$ = $2; }
                ;

parameter_list:
                parameter { $$ = $1; }
        |       parameter ',' parameter_list { $$ = ext_node($1, 1, $2); }
        |       DOTDOTDOT { $$ = mk_atom(yytext); }
                ;

parameter:
                basic_type declarator { $$ = mk_node("Parameter", 2, $1, $2); }
        |       inout basic_type declarator { $$ = mk_node("Parameter", 3, $1, $2, $3); }
        |       basic_type declarator DOTDOTDOT { $$ = mk_node("Parameter", 3, $1, $2, $3); }
        |       inout basic_type declarator DOTDOTDOT { $$ = mk_node("Parameter", 4, $1, $2, $3, $4); }
        |       basic_type declarator '=' assign_expression { $$ = mk_node("Parameter", 3, $1, $2, $4); }
        |       inout basic_type declarator '=' assign_expression { $$ = mk_node("Parameter", 4, $1, $2, $3, $5); }
        |       type { $$ = mk_node("Parameter", 1, $1); }
        |       inout type { $$ = mk_node("Parameter", 2, $1, $2); }
        |       type DOTDOTDOT { $$ = mk_node("Parameter", 2, $1, $2); }
        |       inout type DOTDOTDOT { $$ = mk_node("Parameter", 3, $1, $2, $3); }
                ;

inout:
                inout_x { $$ = $1; }
        |       inout inout_x { $$ = ext_node($1, 1, $2); }
                ;

inout_x:
                AUTO { $$ = mk_atom(yytext); }
        |       type_ctor { $$ = $1; }
        |       FINAL { $$ = mk_atom(yytext); }
        |       IN { $$ = mk_atom(yytext); }
        |       LAZY { $$ = mk_atom(yytext); }
        |       OUT { $$ = mk_atom(yytext); }
        |       REF { $$ = mk_atom(yytext); }
        |       RETURN REF { $$ = mk_atom(yytext); }
        |       SCOPE { $$ = mk_atom(yytext); }
                ;

function_attributes:
                function_attribute { $$ = $1; }
        |       function_attribute function_attributes { $$ = ext_node($1, 1, $2); }
                ;

function_attribute:
                NOTHROW { $$ = mk_atom(yytext); }
        |       PURE { $$ = mk_atom(yytext); }
                ;

member_function_attributes:
                member_function_attribute { $$ = $1; }
        |       member_function_attribute member_function_attributes { $$ = ext_node($1, 1, $2); }
                ;

member_function_attribute:
                CONST { $$ = mk_atom(yytext); }
        |       IMMUTABLE { $$ = mk_atom(yytext); }
        |       INOUT { $$ = mk_atom(yytext); }
        |       RETURN { $$ = mk_atom(yytext); }
        |       SHARED { $$ = mk_atom(yytext); }
        |       function_attribute
                ;

function_body:
                block_statement { $$ = $1; }
        |       body_statement { $$ = $1; }
        |       function_contracts { $$ = $1; }
                ;

function_contracts:
                in_statement { $$ = mk_node("FunctionContract", 1, $1); }
        |       in_statement out_statement { $$ = mk_node("FunctionContract", 2, $1, $2); }
        |       out_statement { $$ = mk_node("FunctionContract", 1, $1); }
        |       out_statement in_statement { $$ = mk_node("FunctionContract", 2, $1, $2); }
                ;

in_statement:
                IN block_statement { $$ = mk_node("InStatement", 1, $2); }
                ;

out_statement:
                OUT block_statement { $$ = mk_node("OutStatement", 1, $2); }
        |       OUT '(' IDENTIFIER ')' block_statement { $$ = mk_node("OutStatement", 2, $3, $5); }
                ;

body_statement:
                BODY block_statement { $$ = mk_node("BodyStatement", 1, $2); }
        |       DO block_statement { $$ = mk_node("BodyStatement", 1, $2); }
                ;

constructor:
                THIS parameters ';' { $$ = mk_node("Constructor", 1, $2); }
        |       THIS parameters member_function_attributes ';' { $$ = mk_node("Constructor", 2, $2, $3); }
        |       THIS parameters function_body { $$ = mk_node("Constructor", 2, $2, $3); }
        |       THIS parameters member_function_attributes function_body { $$ = mk_node("Constructor", 3, $2, $3, $4); }
                ;

destructor:
                '~' THIS '(' ')' ';' { $$ = mk_node("Destructor", 1, mk_none()); }
        |       '~' THIS '(' ')' member_function_attributes ';' { $$ = mk_node("Destructor", 1, $5); }
        |       '~' THIS '(' ')' function_body { $$ = mk_node("Destructor", 1, $5); }
        |       '~'THIS '(' ')' member_function_attributes function_body { $$ = mk_node("Destructor", 2, $5, $6); }
                ;

postblit:
                THIS '(' THIS ')' ';' { $$ = mk_node("Postblit", 1, mk_none()); }
        |       THIS '(' THIS ')' member_function_attributes ';' { $$ = mk_node("Postblit", 1, $5); }
        |       THIS '(' THIS ')' function_body { $$ = mk_node("Postblit", 1, $5); }
        |       THIS '(' THIS ')' member_function_attributes function_body { $$ = mk_node("Postblit", 2, $5, $6); }
                ;

allocator:
                NEW parameters ';' { $$ = mk_node("Allocator", 1, $2); }
        |       NEW parameters function_body { $$ = mk_node("Allocator", 2, $2, $3); }
                ;

deallocator:
                DELETE parameters ';' { $$ = mk_node("Deallocator", 1, $2); }
        |       DELETE parameters function_body { $$ = mk_node("Deallocator", 2, $2, $3); }
                ;

invariant:
                INVARIANT '(' ')' block_statement { $$ = mk_node("Invariant", 1, $4); }
        |       INVARIANT block_statement { $$ = mk_node("Invariant", 1, $2); }
                ;

unittest:
                UNITTEST block_statement { $$ = mk_node("Unittest", 1, $2); }
                ;

static_constructor:
                STATIC THIS '(' ')' ';' { $$ = mk_node("StaticConstructor", 1, mk_none()); }
        |       STATIC THIS '(' ')' member_function_attributes ';' { $$ = mk_node("StaticConstructor", 1, $5); }
        |       STATIC THIS '(' ')' function_body { $$ = mk_node("StaticConstructor", 1, $5); }
        |       STATIC THIS '(' ')' member_function_attributes function_body { $$ = mk_node("StaticConstructor", 2, $5, $6); }
                ;

static_destructor:
                STATIC '~' THIS '(' ')' ';' { $$ = mk_node("StaticDestructor", 1, mk_none()); }
        |       STATIC '~' THIS '(' ')' member_function_attributes ';' { $$ = mk_node("StaticDestructor", 1, $6); }
        |       STATIC '~' THIS '(' ')' function_body { $$ = mk_node("StaticDestructor", 1, $6); }
        |       STATIC '~' THIS '(' ')' member_function_attributes function_body { $$ = mk_node("StaticDestructor", 2, $6, $7); }
                ;

shared_static_constructor:
                SHARED STATIC THIS '(' ')' ';' { $$ = mk_node("SharedStaticConstructor", 1, mk_none()); }
        |       SHARED STATIC THIS '(' ')' member_function_attributes ';' { $$ = mk_node("SharedStaticThis", 1, $6); }
        |       SHARED STATIC THIS '(' ')' function_body { $$ = mk_node("SharedStaticConstructor", 1, $6); }
        |       SHARED STATIC THIS '(' ')' member_function_attributes function_body { $$ = mk_node("SharedStaticThis", 2, $6, $7); }
                ;

shared_static_destructor:
                SHARED STATIC '~' THIS '(' ')' ';' { $$ = mk_node("SharedStaticThis", 1, mk_none()); }
        |       SHARED STATIC '~' THIS '(' ')' member_function_attributes ';' { $$ = mk_node("SharedStaticThis", 1, $7); }
        |       SHARED STATIC '~' THIS '(' ')' function_body { $$ = mk_node("SharedStaticThis", 1, $7); }
        |       SHARED STATIC '~' THIS '(' ')' member_function_attributes function_body { $$ = mk_node("SharedStaticThis", 2, $7, $8); }
                ;

/**
Aggregate.
*/

aggregate_declaration:
                class_declaration { $$ = $1; }
        |       interface_declaration { $$ = $1; }
        |       struct_declaration { $$ = $1; }
        |       union_declaration { $$ = $1; }
                ;

class_declaration:
                CLASS IDENTIFIER ';' { $$ = mk_node("ClassDeclaration", 1, $2); }
        |       CLASS IDENTIFIER aggregate_body { $$ = mk_node("ClassDeclaration", 2, $2, $3); }
        |       CLASS IDENTIFIER base_class_list aggregate_body { $$ = mk_node("ClassDeclaration", 3, $2, $3, $4); }
        |       class_template_declaration { $$ = $1; }
                ;

class_template_declaration:
                CLASS IDENTIFIER template_parameters aggregate_body { $$ = mk_node("ClassDeclaration", 3, $2, $3, $4); }
        |       CLASS IDENTIFIER template_parameters constraint aggregate_body { $$ = mk_node("ClassDeclaration", 4, $2, $3, $4, $5); }
        |       CLASS IDENTIFIER template_parameters base_class_list aggregate_body { $$ = mk_node("ClassDeclaration", 4, $2, $3, $4, $5); }
        |       CLASS IDENTIFIER template_parameters constraint base_class_list aggregate_body { $$ = mk_node("ClassDeclaration", 5, $2, $3, $4, $5, $6); }
        |       CLASS IDENTIFIER template_parameters base_class_list constraint aggregate_body { $$ = mk_node("ClassDeclaration", 5, $2, $3, $4, $5, $6); }
                ;

interface_declaration:
                INTERFACE IDENTIFIER ';' { $$ = mk_node("InterfaceDeclaration", 1, $2); }
        |       INTERFACE IDENTIFIER aggregate_body { $$ = mk_node("InterfaceDeclaration", 2, $2, $3); }
        |       INTERFACE IDENTIFIER base_interface_list aggregate_body { $$ = mk_node("InterfaceDeclaration", 3, $2, $3, $4); }
        |       interface_template_declaration { $$ = $1; }
                ;

interface_template_declaration:
                INTERFACE IDENTIFIER template_parameters aggregate_body { $$ = mk_node("InterfaceDeclaration", 3, $2, $3, $4); }
        |       INTERFACE IDENTIFIER template_parameters constraint aggregate_body { $$ = mk_node("InterfaceDeclaration", 4, $2, $3, $4, $5); }
        |       INTERFACE IDENTIFIER template_parameters base_interface_list aggregate_body { $$ = mk_node("InterfaceDeclaration", 4, $2, $3, $4, $5); }
        |       INTERFACE IDENTIFIER template_parameters constraint base_interface_list aggregate_body { $$ = mk_node("InterfaceDeclaration", 5, $2, $3, $4, $5, $6); }
        |       INTERFACE IDENTIFIER template_parameters base_interface_list constraint aggregate_body { $$ = mk_node("InterfaceDeclaration", 5, $2, $3, $4, $5, $6); }
                ;

struct_declaration:
                STRUCT IDENTIFIER ';' { $$ = mk_node("StructDeclaration", 1, $2); }
        |       STRUCT IDENTIFIER aggregate_body { $$ = mk_node("StructDeclaration", 2, $2, $3); }
        |       struct_template_declaration { $$ = $1; }
        |       anon_struct_declaration { $$ = $1; }
                ;

struct_template_declaration:
                STRUCT IDENTIFIER template_parameters aggregate_body { $$ = mk_node("StructDeclaration", 3, $2, $3, $4); }
        |       STRUCT IDENTIFIER template_parameters constraint aggregate_body { $$ = mk_node("StructDeclaration", 4, $2, $3, $4, $5); }
                ;

anon_struct_declaration:
                STRUCT aggregate_body { $$ = mk_node("AnonStructDeclaration", 1, $2); }
                ;

union_declaration:
                UNION IDENTIFIER ';' { $$ = mk_node("UnionDeclaration", 1, $2); }
        |       UNION IDENTIFIER aggregate_body { $$ = mk_node("UnionDeclaration", 2, $2, $3); }
        |       union_template_declaration { $$ = $1; }
        |       anon_union_declaration { $$ = $1; }
                ;

union_template_declaration:
                UNION IDENTIFIER template_parameters aggregate_body { $$ = mk_node("UnionDeclaration", 3, $2, $3, $4); }
        |       UNION IDENTIFIER template_parameters constraint aggregate_body { $$ = mk_node("UnionDeclaration", 4, $2, $3, $4, $5); }
                ;

anon_union_declaration:
                UNION aggregate_body { $$ = mk_node("AnonUnionDeclaration", 1, $2); }
                ;

aggregate_body:
                '{' '}' { mk_none(); }
        |       '{' decl_defs '}' { $$ = $2; }
                ;

base_class_list:
                ':' super_class
        |       ':' super_class ',' interfaces
        |       ':' interfaces
                ;

base_interface_list:
                ':' interfaces { $$ = $2; }
                ;

super_class:
                basic_type { $$ = mk_node("SuperClass", 1, $1); }
                ;

interfaces:
                interface { $$ = $1; }
        |       interface ',' interfaces { $$ = ext_node($1, 1, $3); }
                ;

interface:
                basic_type { $$ = mk_node("Interface", 1, $1); }
                ;

alias_this:
                ALIAS IDENTIFIER THIS ';' { $$ = mk_node("AliasThis", 1, $2); }
                ;

/**
Enum.
*/

enum_declaration:
                ENUM IDENTIFIER enum_body { $$ = mk_node("EnumDeclaration", 2, $2, $3); }
        |       ENUM IDENTIFIER ':' enum_base_type enum_body { $$ = mk_node("EnumDeclaration", 3, $2, $4, $5); }
        |       anonymous_enum_declaration { $$ = $1; }
                ;

enum_base_type:
                type { $$ = $1; }
                ;

enum_body:
                '{' enum_members '}' { $$ = mk_node("EnumBody", 1, $2); }
                ;

enum_members:
                enum_member { $$ = $1; }
        |       enum_member ',' { $$ = $1; }
        |       enum_member ',' enum_members { $$ = ext_node($1, 1, $3); }
                ;

enum_member:
                IDENTIFIER
        |       IDENTIFIER '=' assign_expression
                ;

anonymous_enum_declaration:
                ENUM ':' enum_base_type '{' enum_members '}' { $$ = mk_node("AnonymousEnumDeclaration", 2, $3, $5); }
        |       ENUM '{' enum_members '}' { $$ = mk_node("AnonymousEnumDeclaration", 1, $3); }
        |       ENUM '{' anonymous_enum_members '}' { $$ = mk_node("AnonymousEnumDeclaration", 1, $3); }
                ;

anonymous_enum_members:
                anonymous_enum_member { $$ = $1; }
        |       anonymous_enum_member ',' { $$ = $1; }
        |       anonymous_enum_member ',' anonymous_enum_members { $$ = ext_node($1, 1, $3); }
                ;

anonymous_enum_member:
                enum_member { $$ = $1; }
        |       type IDENTIFIER '=' assign_expression { $$ = mk_node("AnonymousEnumMember", 3, $1, $2, $4); }
                ;

/**
Template.
*/

template_declaration:
                TEMPLATE IDENTIFIER template_parameters '{' '}'
        |       TEMPLATE IDENTIFIER template_parameters constraint '{' '}'
        |       TEMPLATE IDENTIFIER template_parameters '{' decl_defs '}'
        |       TEMPLATE IDENTIFIER template_parameters constraint '{' decl_defs '}'
                ;

template_parameters:
                '(' ')' { $$ = mk_node("TemplateParameters", 1, mk_none()); }
        |       '(' template_parameter_list ')' {$$ = mk_node("TemplateParameters", 1, $2); }
                ;

template_parameter_list:
                template_parameter { $$ = $1; }
        |       template_parameter ',' { $$ = 1; }
        |       template_parameter ',' template_parameter_list { $$ = ext_node($1, 1, $3); }
                ;

template_parameter:
                template_type_parameter { $$ = $1; }
        |       template_value_parameter { $$ = $1; }
        |       template_alias_parameter { $$ = $1; }
        |       template_tuple_parameter { $$ = $1; }
        |       template_this_parameter { $$ = $1; }
                ;

constraint:
               IF '(' expression ')' { $$ = mk_node("Constraint", 1, $3); }
                ;

template_instance:
                IDENTIFIER template_arguments
                ;

template_arguments:
                '!' '(' ')'
        |       '!' '(' template_argument_list ')'
        |       '!' template_single_argument { $$ = mk_node("TemplateArguments", 1, $2); }
                ;

template_argument_list:
                template_argument { $$ = $1; }
        |       template_argument ',' { $$ = $1; }
        |       template_argument ',' template_argument_list
                ;

template_argument:
                type { $$ = $1; }
        |       assign_expression { $$ = $1; }
        |       symbol { $$ = $1; }
                ;

symbol:
                symbol_tail { $$ = mk_node("Symbol", 1, $1); }
        |       '.' symbol_tail { $$ = mk_node("Symbol", 1, $2); }
                ;

symbol_tail:
                IDENTIFIER
        |       IDENTIFIER '.' symbol_tail
        |       template_instance
        |       template_instance '.' symbol_tail
                ;

template_single_argument:
                IDENTIFIER { $$ = mk_atom(yytext); }
        |       basic_type_x { $$ = $1; }
        /* |       character_literal */
        |       DOUBLEQOUTEDSTRING { $$ = mk_atom(yytext); }
        |       WYSIWYGSTRING { $$ = mk_atom(yytext); }
        |       TOKENSTRING { $$ = mk_atom(yytext); }
        |       INTEGER { $$ = mk_atom(yytext); }
        |       FLOATINGPOINT { $$ = mk_atom(yytext); }
        |       TRUE { $$ = mk_atom(yytext); }
        |       FALSE { $$ = mk_atom(yytext); }
        |       NULLKEYWORD { $$ = mk_atom(yytext); }
        |       THIS { $$ = mk_atom(yytext); }
                ;

template_type_parameter:
                IDENTIFIER
        |       IDENTIFIER template_type_parameter_specialization
        |       IDENTIFIER template_type_parameter_default
        |       IDENTIFIER template_type_parameter_specialization template_type_parameter_default
                ;

template_type_parameter_specialization:
                ':' type { $$ = mk_node("TemplateTypeParameterSpecialization", 1, $2); }
                ;

template_type_parameter_default:
                '=' type { $$ = mk_node("TemplateTypeParameterDefault", 1, $2); }
                ;

template_this_parameter:
                THIS template_type_parameter
                ;

template_value_parameter:
                basic_type declarator
        |       basic_type declarator template_value_parameter_specialization
        |       basic_type declarator template_value_parameter_default
        |       basic_type declarator template_value_parameter_specialization template_value_parameter_default
                ;

template_value_parameter_specialization:
                ':' conditional_expression
                ;

template_value_parameter_default:
                '=' assign_expression
                ;

template_alias_parameter:
                ALIAS IDENTIFIER
        |       ALIAS IDENTIFIER template_alias_parameter_specialization
        |       ALIAS IDENTIFIER template_alias_parameter_default
        |       ALIAS IDENTIFIER template_alias_parameter_specialization template_alias_parameter_default
        |       ALIAS basic_type declarator
        |       ALIAS basic_type declarator template_alias_parameter_specialization
        |       ALIAS basic_type declarator template_alias_parameter_default
        |       ALIAS basic_type declarator template_alias_parameter_specialization template_alias_parameter_default
                ;

template_alias_parameter_specialization:
                ':' type
        |       ':' conditional_expression
                ;

template_alias_parameter_default:
                '=' type
        |       '=' conditional_expression
                ;

template_tuple_parameter:
                IDENTIFIER DOTDOTDOT
                ;

template_mixin_declaration:
                MIXIN TEMPLATE IDENTIFIER template_parameters '{' '}'
        |       MIXIN TEMPLATE IDENTIFIER template_parameters constraint '{' '}'
        |       MIXIN TEMPLATE IDENTIFIER template_parameters '{' decl_defs '}'
        |       MIXIN TEMPLATE IDENTIFIER template_parameters constraint '{' decl_defs '}'
                ;

template_mixin:
                MIXIN mixin_template_name ';'
        |       MIXIN mixin_template_name template_arguments ';'
        |       MIXIN mixin_template_name IDENTIFIER ';'
        |       MIXIN mixin_template_name template_arguments IDENTIFIER ';'
                ;

mixin_template_name:
                '.' qualified_identifier_list
        |       qualified_identifier_list
        |       typeof '.' qualified_identifier_list
                ;

qualified_identifier_list:
                IDENTIFIER
        |       IDENTIFIER '.' qualified_identifier_list
        |       template_instance '.' qualified_identifier_list
                ;

/**
Attribute.
*/

attribute_specifier:
                attribute ':' { mk_node("AttributeSpecifier", 1, $1); }
        |       attribute declaration_block { mk_node("AttributeSpecifier", 2, $1, $2); }
        ;

attribute:
                align_attribute { $$ = $1; }
        |       deprecated_attribute { $$ = $1; }
        |       protection_attribute { $$ = $1; }
        |       pragma { $$ = $1; }
        |       STATIC { $$ = mk_atom(yytext); }
        |       EXTERN { $$ = mk_atom(yytext); }
        |       ABSTRACT { $$ = mk_atom(yytext); }
        |       FINAL { $$ = mk_atom(yytext); }
        |       OVERRIDE { $$ = mk_atom(yytext); }
        |       SYNCHRONIZED { $$ = mk_atom(yytext); }
        |       AUTO { $$ = mk_atom(yytext); }
        |       SCOPE { $$ = mk_atom(yytext); }
        |       CONST { $$ = mk_atom(yytext); }
        |       IMMUTABLE { $$ = mk_atom(yytext); }
        |       INOUT { $$ = mk_atom(yytext); }
        |       SHARED { $$ = mk_atom(yytext); }
        |       SPECIAL_GSHARED { $$ = mk_atom(yytext); }
        |       NOTHROW { $$ = mk_atom(yytext); }
        |       PURE { $$ = mk_atom(yytext); }
        |       REF { $$ = mk_atom(yytext); }
        ;

declaration_block:
                decl_def
        |       '{' '}' {$$ = mk_none(); }
        |       '{' decl_defs '}'
        ;

align_attribute:
                ALIGN { $$ = mk_node("AlignAttribute", 1, mk_none()); }
        |       ALIGN '(' assign_expression ')' { $$ = mk_node("AlignAttribute", 1, $3); }
        ;

deprecated_attribute:
                DEPRECATED { $$ = mk_node("DeprecatedAttribute", 1, mk_none()); }
        |       DEPRECATED '(' assign_expression ')' { $$ = mk_node("DeprecatedAttribute", 1, $3); }
        ;

protection_attribute:
                PRIVATE { $$ = mk_node("PrivateAttribute", 1, mk_none()); }
        |       PACKAGE { $$ = mk_node("PackageAttribute", 1, mk_none()); }
        |       PACKAGE '(' identifier_list ')' { $$ = mk_node("PackageAttribute", 1, $3); }
        |       PROTECTED { $$ = mk_node("ProtectedAttribute", 1,mk_none()); }
        |       PUBLIC { $$ = mk_node("PublicAttribute", 1, mk_none()); }
        |       EXPORT { $$ = mk_node("ExportAttribute", 1, mk_none()); }
        ;

property:
                '@' property_identifier { $$ = mk_node("Property", 1, $2); }
        |       user_defined_attribute { $$ = mk_node("Property", 1, $1); }
        ;

property_identifier:
                PROPERTY { $$ = mk_atom(yytext); }
        |       SAFE { $$ = mk_atom(yytext); }
        |       TRUSTED { $$ = mk_atom(yytext); }
        |       SYSTEM { $$ = mk_atom(yytext); }
        |       DISABLE { $$ = mk_atom(yytext); }
        |       NOGC { $$ = mk_atom(yytext); }
                ;

user_defined_attribute:
                '@' '(' argument_list ')' { mk_node("UserDefinedAttribute", 1, $3); }
        |       '@' IDENTIFIER { mk_node("UserDefinedAttribute", 1, $2); }
        |       '@' IDENTIFIER '(' ')' { mk_node("UserDefinedAttribute", 1, $2); }
        |       '@'IDENTIFIER '(' argument_list ')' { mk_node("UserDefinedAttribute", 2, $2, $4); }
        |       '@' template_instance { mk_node("UserDefinedAttribute", 1, $2); }
        |       '@' template_instance '(' ')' { mk_node("UserDefinedAttribute", 1, $2); }
        |       '@'template_instance '(' argument_list ')' { mk_node("UserDefinedAttribute", 2, $2, $4); }
        ;

pragma:
                PRAGMA '(' IDENTIFIER ')' { $$ = mk_node("Praagma", 1, $3); }
        |       PRAGMA '(' IDENTIFIER ',' argument_list ')' { $$ = mk_node("Pragma", 2, $3, $5); }
        ;

/**
Conditional.
*/

conditional_declaration:
                condition declaration_block { $$ = mk_node("ConditionalDeclaration", 2, $1, $2); }
        |       condition declaration_block ELSE declaration_block { $$ = mk_node("ConditionalDeclaration", 3, $1, $2, $4); }
        |       condition ':' { $$ = mk_node("ConditionalDeclaration", 1, $1); }
        |       condition ':' decl_defs { $$ = mk_node("ConditionalDeclaration", 2, $1, $3); }
        |       condition declaration_block ELSE ':' { $$ = mk_node("ConditionalDeclaration", 2, $1, $2); }
        |       condition declaration_block ELSE ':' decl_defs { $$ = mk_node("ConditionalDeclaration", 3, $1, $2, $5); }
                ;

conditional_statement:
                condition no_scope_non_empty_statement { $$ = mk_node("ConditionalStatement", 2, $1, $2); }
        |       condition no_scope_non_empty_statement ELSE no_scope_non_empty_statement { $$ = mk_node("ConditionalStatement", 3, $1, $2, $4); }
                ;

condition:
                version_condition { $$ = $1; }
        |       debug_condition { $$ = $1; }
        |       static_if_condition { $$ = $1; }
                ;

version_condition:
                VERSION '(' INTEGER ')' { $$ = mk_node("VersionCondition", 1, $3); }
        |       VERSION '(' IDENTIFIER ')' { $$ = mk_node("VersionCondition", 1, $3); }
        |       VERSION '(' UNITTEST ')' { $$ = mk_node("VersionCondition", 1, $3); }
        |       VERSION '(' ASSERT ')' { $$ = mk_node("VersionCondition", 1, $3); }
                ;

debug_condition:
                DEBUG { $$ = mk_node("DebugCondition", 1, mk_none()); }
        |       DEBUG '(' INTEGER ')' { $$ = mk_node("DebugCondition", 1, $3); }
        |       DEBUG '(' IDENTIFIER ')' { $$ = mk_node("Debugcondition", 1, $3); }
                ;

static_if_condition:
                STATIC IF '(' assign_expression ')' { $$ = mk_node("StaticIfCondition", 1, $4); }
                ;

version_specification:
                VERSION '=' IDENTIFIER ';' { $$ = mk_node("VersionSpecification", 1, $3); }
        |       VERSION '=' INTEGER ';' { $$ = mk_node("VersionSpecification", 1, $3); }
                ;

debug_specification:
                DEBUG '=' IDENTIFIER ';' { $$ = mk_node("DebugSpecification", 1, $3); }
        |       DEBUG '=' INTEGER ';' { $$ = mk_node("DebugSpecification", 1, $3); }
                ;

static_foreach:
                STATIC aggregate_foreach { $$ = mk_node("StaticForeach", 1, $2); }
        |       STATIC range_foreach { $$ = mk_node("StaticForeach", 1, $2); }
                ;

static_foreach_declaration:
                static_foreach declaration_block { $$ = mk_node("StaticForeachDeclaration", 2, $1, $2); }
        |       static_foreach ':' { $$ = mk_node("StaticForeachDeclaration", 1, $1); }
        |       static_foreach ':' decl_defs { $$ = mk_node("StaticForeachDeclaration", 2, $1, $2); }
                ;

static_foreach_statement:
                static_foreach no_scope_non_empty_statement { $$ = mk_node("StaticForeachStatement", 2, $1, $2); }
                ;

static_assert:
                STATIC ASSERT '(' assign_expression ')' { $$ = mk_node("StaticAssert", 1, $4); }
        |       STATIC ASSERT '(' assign_expression ',' ')' { $$ = mk_node("StaticAssert", 1, $4); }
        |       STATIC ASSERT '(' assign_expression ',' assign_expression ')' { $$ = mk_node("StaticAssert", 2, $4, $6); }
        |       STATIC ASSERT '(' assign_expression ',' assign_expression ',' ')' { $$ = mk_node("StaticAssert", 2, $4, $6); }
                ;

/**
Module.
*/

module:
                module_declaration decl_defs { $$ = mk_node("Module", 2, $1, $2); }
        |       decl_defs { $$ = mk_node("Module", 1, $1); }
        ;

decl_defs:
                decl_def { $$ = $1; }
        |       decl_def decl_defs { $$ = ext_node($1, 1, $2); }
        ;

decl_def:
                attribute_specifier { $$ = $1; }
        |       declaration { $$ = $1; }
        |       constructor { $$ = $1; }
        |       destructor { $$ = $1; }
        |       postblit { $$ = $1; }
        |       allocator { $$ = $1; }
        |       deallocator { $$ = $1; }
        |       invariant { $$ = $1; }
        |       unittest { $$ = $1; }
        |       alias_this { $$ = $1; }
        |       static_constructor { $$ = $1; }
        |       static_destructor { $$ = $1; }
        |       shared_static_constructor { $$ = $1; }
        |       shared_static_destructor { $$ = $1; }
        |       debug_specification { $$ = $1; }
        |       version_specification { $$ = $1; }
        |       template_declaration { $$ = $1; }
        |       template_mixin_declaration { $$ = $1; }
        |       template_mixin { $$ = $1; }
        |       mixin_declaration { $$ = $1; }
        |       ';' {$$ = mk_none(); }
        ;

module_declaration:
                MODULE module_fully_qualified_name ';' { $$ = mk_node("ModuleDeclaration", 1, $2); }
        |       module_attributes MODULE module_fully_qualified_name ';' { $$ = mk_node("ModuleDeclaration", 2, $1, $3); }
        ;

module_attributes:
                module_attribute { $$ = $1; }
        |       module_attribute module_attributes { $$ = ext_node($1, 1, $2); }
        ;

module_attribute:
                deprecated_attribute { $$ = $1; }
        |       user_defined_attribute { $$ = $1; }
        ;

module_fully_qualified_name:
                module_name { $$ = $1; }
        |       packages '.' module_name
        ;

module_name:
                IDENTIFIER { $$ = mk_node("ModuleName", 1, $1); }
        ;

packages:
                package_name
        |       packages '.' package_name
        ;

package_name:
                 IDENTIFIER { $$ = mk_node("PackageName", 1, $1); }
        ;

import_declaration:
                IMPORT import_list ';' { $$ = mk_node("ImportDeclaration", 1, $2); }
        |       STATIC IMPORT import_list ';' { $$ = mk_node("ImportDeclaration", 1, $3); }
        ;

import_list:
                import { $$ = $1; }
        |       import_bindings
        |       import ',' import_list
        ;

import:
                module_fully_qualified_name
        |       module_alias_identifier '=' module_fully_qualified_name
        ;

import_bindings:
                import ':' import_bind_list
        ;

import_bind_list:
                import_bind
        |       import_bind ',' import_bind_list
        ;

import_bind:
                IDENTIFIER
        |       IDENTIFIER '=' IDENTIFIER
        ;

module_alias_identifier:
                IDENTIFIER
        ;

mixin_declaration:
                MIXIN '(' assign_expression ')' ';' { $$ = mk_node("MixinDeclaration", 1, $3); }
        ;
