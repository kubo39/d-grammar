%{
#include <stdio.h>

%}

%option stack
%option yylineno

%x alternativewysiwygstring
%x blockcomment
%x doublequotedstring
%x hexstring
%x linecomment
%x nestingblockcomment
%x suffix
%x tokenstring
%x wysiwygstring

identifier [_a-zA-Z][_a-zA-Z0-9]*

%%

<suffix>{identifier}      { BEGIN(INITIAL); }
<suffix>(.|\n) { yyless(0); BEGIN(INITIAL); }

[\x20\x09\x0B\x0C]  { }

\/\/                                 { BEGIN(linecomment); }
<linecomment>[\x0D\x0A\x2028\x2029]  { BEGIN(INITIAL); }
<linecomment>\x0D\x0A                { BEGIN(INITIAL); }
<linecomment>.                       { }

\/\*                 { yy_push_state(blockcomment); }
<blockcomment>\*\/   { yy_pop_state(); }
<blockcomment>.      { }

\/\+                        { yy_push_state(nestingblockcomment); }
<nestingblockcomment>\/\+   { yy_push_state(nestingblockcomment); }
<nestingblockcomment>\+\/   { yy_pop_state(); }
<nestingblockcomment>.      { }

abstract { return ABSTRACT; }
alias    { return ALIAS; }
align    { return ALIGN; }
asm      { return ASM; }
assert   { return ASSERT; }
auto     { return AUTO; }
body     { return BODY; }
bool     { return BOOL; }
break    { return BREAK; }
byte     { return BYTE; }
case     { return CASE; }
cast     { return CAST; }
catch    { return CATCH; }
cdouble  { return CDOUBLE; }
cent     { return CENT; }
cfloat   { return CFLOAT; }
char     { return CHAR; }
class    { return CLASS; }
const    { return CONST; }
contine  { return CONTINUE; }
creal    { return CREAL; }
dchar    { return DCHAR; }
debug    { return DEBUG; }
defaut   { return DEFAULT; }
delegate { return DELEGATE; }
delete   { return DELETE; }
deprecated { return DEPRECATED; }
do       { return DO; }
double   { return DOUBLE; }
else     { return ELSE; }
enum     { return ENUM; }
export   { return EXPORT; }
extern   { return EXTERN; }
false    { return FALSE; }
final    { return FINAL; }
finally  { return FINALLY; }
float    { return FLOAT; }
for      { return FOR; }
foreach  { return FOREACH; }
foreach_reverse  { return FOREACHREVERSE; }
function  { return FUNCTION; }
goto  { return GOTO; }
idouble  { return IDOUBLE; }
if  { return IF; }
ifloat  { return IFLOAT; }
immutable  { return IMMUTABLE; }
import  { return IMPORT; }
in  { return IN; }
inout  { return INOUT; }
int  { return INT; }
interface  { return INTERFACE; }
invariant  { return INVARIANT; }
ireal  { return IREAL; }
is  { return IS; }
lazy  { return LAZY; }
long  { return LONG; }
macro  { return MACRO; }
mixin  { return MIXIN; }
module  { return MODULE; }
new  { return NEW; }
nothrow  { return NOTHROW; }
null  { return NULLKEYWORD; }
out  { return OUT; }
override  { return OVERRIDE; }
package  { return PACKAGE; }
pragma  { return PRAGMA; }
private  { return PRIVATE; }
protected  { return PROTECTED; }
public  { return PUBLIC; }
pure  { return PURE; }
real  { return REAL; }
ref  { return REF; }
return  { return RETURN; }
scope  { return SCOPE; }
shared  { return SHARED; }
short  { return SHORT; }
static  { return STATIC; }
struct  { return STRUCT; }
super  { return SUPER; }
switch  { return SWITCH; }
synchronized  { return SYNCHRONIZED; }
template  { return TEMPLATE; }
this  { return THIS; }
throw  { return THROW; }
true  { return TRUE; }
try  { return TRY; }
typedef  { return TYPEDEF; }
typeid  { return TYPEID; }
typeof  { return TYPEOF; }
ubyte  { return UBYTE; }
ucent  { return UCENT; }
uint  { return UINT; }
ulong  { return ULONG; }
union  { return UNION; }
unittest  { return UNITTEST; }
ushort  { return USHORT; }
version  { return VERSION; }
void  { return VOID; }
volatile  { return VOLATILE; }
wchar  { return WCHAR; }
while  { return WHILE; }
with  { return WITH; }

__FILE__  { return SPECIAL_FILE; }
__FILE_FULL_PATH__  { return SPECIAL_FILEFULLPATH; }
__MODULE__  { return SPECIAL_MODULE; }
__LINE__  { return SPECIAL_LINE; }
__FUNCTION__  { return SPECIAL_FUNCTION; }
__PRETTY_FUNCTION__  { return SPECIAL_PRETTYFUNCION; }
__gshared  { return SPECIAL_GSHARED; }
__traits  { return SPECIAL_TRAITS; }
__vector  { return SPECIAL_VECTOR; }
__parameters  { return SPECIAL_PARAMETERS; }

isAbstractClass { return TRAITS_ISABSTRACTCLASS; }
isArithmetic { return TRAITS_ISARITHMETIC; }
isAssociativeArray { return TRAITS_ISASSOCIATIVEARRAY; }
isFinalClass { return TRAITS_ISFINALCLASS; }
isPOD { return TRAITS_ISPOD; }
isNested { return TRAITS_ISNESTED; }
isFloating { return TRAITS_ISFLOATING; }
isIntegral { return TRAITS_INTEGRAL; }
isScalar { return TRAITS_ISSCALAR; }
isStaticArray { return TRAITS_ISSTATICARRAY; }
isUnsigned { return TRAITS_ISUNSIGNED; }
isVirtualFunction { return TRAITS_ISVIRTUALFUNCTION; }
isVirtualMethod { return TRAITS_ISVIRTUALMETHOD; }
isAbstractFunction { return TRAITS_ISABSTRACTFUNCTION; }
isFinalFunction { return TRAITS_ISFINALFUNCTION; }
isStaticFunction { return TRAITS_ISSTATICFUNCTION; }
isOverrideFunction { return TRAITS_ISOVERRIDEFUNCTION; }
isTemplate { return TRAITS_ISTEMPLATE; }
isRef { return TRAITS_ISREF; }
isOut { return TRAITS_ISOUT; }
isLazy { return TRAITS_ISLAZY; }
hasMember { return TRAITS_HASMANY; }
identifier { return TRAITS_IDENTIFIER; }
getAliasThis { return TRAITS_GETALIASTHIS; }
getAttributes { return TRAITS_GETATTRIBUTES; }
getFunctionAttributes { return TRAITS_GETFUNCTIONATTRIBUTES; }
getFunctionVariadicStyle { return TRAITS_GETFUNCTIONVARIADICSTYLE; }
getLinkage { return TRAITS_GETLINKAGE; }
getMember { return TRAITS_GETMEMBER; }
getOverloads { return TRAITS_GETOVERLOADS; }
getParameterStorageClasses { return TRAITS_GETPARAMETERSTORAGECLASSES; }
getPointerBitmap { return TRAITS_GETPOINTERBITMAP; }
getProtection { return TRAITS_GETPROTECTION; }
getVirtualFunctions { return TRAITS_GETVIRTUALFUNCTIONS; }
getVirtualMethods { return TRAITS_GETVIRTUALMETHODS; }
getUnitTests { return TRAITS_UNITTEST; }
parent { return TRAITS_PARENT; }
classInstanceSize { return TRAITS_CLASSINSTANCESIZE; }
getVirtualIndex { return TRAITS_GETVIRTUALINDEX; }
allMembers { return TRAITS_ALLMEMBERS; }
derivedMembers { return TRAITS_DERIVEDMEMBERS; }
isSame { return TRAITS_ISSAME; }
compiles { return TRAITS_COMPILES; }

property { return PROPERTY; }
safe { return SAFE; }
trusted { return TRUSTED; }
system { return SYSTEM; }
disable { return DISABLE; }
nogc { return NOGC; }

{identifier} { return IDENTIFIER; }

0[xX][0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*([uU]?L?|L?[uU]?) { return INTEGER; }
0[bB][01_]?([uU]?L?|L?[uU]?)        { return INTEGER; }
[0-9]([uU]?L?|L?[uU]?)              { return INTEGER; }
[1-9][0-9_]*([uU]?L?|L?[uU]?)       { return INTEGER; }

[0-9]\.[0-9]*([fFL]?i?)         { return FLOATINGPOINT; }
[1-9][0-9_]+\.[0-9]*([fFL]?i?)  { return FLOATINGPOINT; }
0[0-9][0-9_]+\.[0-9]*([fFL]?i?) { return FLOATINGPOINT; }
0[0-9_]+[0-9]\.[0-9]*([fFL]?i?) { return FLOATINGPOINT; }

[0-9]+\.[0-9]+[0-9_]*[eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }

0[xX][0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*\.[0-9a-fA-F][0-9a-fA-F_]*p(\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }
0[xX]\.?[0-9a-fA-F][0-9a-fA-F_]*p(\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }

\.[0-9]([fFL]?i?)                             { return FLOATINGPOINT; }
\.[1-9][0-9_]+([fFL]?i?)                      { return FLOATINGPOINT; }
\.[0-9][eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?)        { return FLOATINGPOINT; }
\.[1-9][0-9_]+[eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }

[0-9][eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?)         { return FLOATINGPOINT; }
[1-9][0-9_][eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?)   { return FLOATINGPOINT; }
0[0-9][0-9_]+[eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }
0[0-9_]+[0-9][eE](\+|\-)?[0-9_]*[0-9][0-9_]*([fFL]?i?) { return FLOATINGPOINT; }

0[xX][0-9a-fA-F_]*([uU]?L?|L?[uU]?)[fFi]  { return FLOATINGPOINT; }
0[bB][01_]*([uU]?L?|L?[uU]?)[fFi]         { return FLOATINGPOINT; }
[0-9]([uU]?L?|L?[uU]?)[fFi]               { return FLOATINGPOINT; }
[1-9][0-9_]*([uU]?L?|L?[uU]?)[fFi]        { return FLOATINGPOINT; }
0[xX][0-9a-fA-F_]*([uU]?L?|L?[uU]?)[fFL]i { return FLOATINGPOINT; }
0[bB][01_]*([uU]?L?|L?[uU]?)[fFL]i        { return FLOATINGPOINT; }
[0-9]([uU]?L?|L?[uU]?)[fFL]i              { return FLOATINGPOINT; }
[1-9][0-9_]*([uU]?L?|L?[uU]?)[fFL]i       { return FLOATINGPOINT; }


x\x22                        { BEGIN(hexstring); yymore(); }
<hexstring>\x22[cwd]?        { BEGIN(suffix); return HEXSTRING; }

<hexstring><<EOF>>                                          { return -1; }
<hexstring>[0-9a-fA-F\x20\x09\x0B\x0C\x0D\x0A\x2028\x2029]  { yymore(); }
<hexstring>[\x00\x1A]                                       { return -1; }
<hexstring>\x0D\x0A                                         { yymore(); }

r\x22                        { BEGIN(wysiwygstring); yymore(); }
<wysiwygstring>\x22[cwd]?    { BEGIN(suffix); return WYSIWYGSTRING; }

<wysiwygstring><<EOF>>                { return -1; }
<wysiwygstring>[\x0D\x0A\x2028\x2029] { yymore(); }
<wysiwygstring>[\x00\x1A]             { return -1; }
<wysiwygstring>.                      { yymore(); }

\x60                                    { BEGIN(alternativewysiwygstring); yymore(); }
<alternativewysiwygstring>\x60[cwd]?    { BEGIN(suffix); return WYSIWYGSTRING; }

<alternativewysiwygstring><<EOF>>                { return -1; }
<alternativewysiwygstring>[\x0D\x0A\x2028\x2029] { yymore(); }
<alternativewysiwygstring>\x0D\x0A               { yymore(); }
<alternativewysiwygstring>[\x00\x1A]             { return -1; }
<alternativewysiwygstring>.                      { yymore(); }

\x22                           { BEGIN(doublequotedstring); yymore(); }
<doublequotedstring>\x22[cwd]? { BEGIN(suffix); return DOUBLEQOUTEDSTRING; }

<doublequotedstring><<EOF>>                               { return -1; }
<doublequotedstring>\\[\x27\x22\?\\\\a\\b\\f\\n\\r\\t\\v] { yymore(); }
<doublequotedstring>\\x[0-9a-fA-F]{2}                     { yymore(); }
<doublequotedstring>\\[0-7]{1,3}                          { yymore(); }
<doublequotedstring>\\u[0-9a-fA-F]{4}                     { yymore(); }
<doublequotedstring>\\U[0-9a-fA-F]{8}                     { yymore(); }
<doublequotedstring>[\x0D\x0A\x2028\x2029]                { yymore(); }
<doublequotedstring>\x0D\x0A                              { yymore(); }
<doublequotedstring>[\x00\x1A]                            { return -1; }
<doublequotedstring>\\[^\x27\x22\?\\\\a\\b\\f\\n\\r\\t\\v] { return -1; }
<doublequotedstring>.                                     { yymore(); }

q\{                          { BEGIN(tokenstring); yymore(); }
<tokenstring>\}              { BEGIN(suffix); return TOKENSTRING; }

<tokenstring><<EOF>>         { return -1; }
<tokenstring>.               { yymore(); }


\/      { return '/'; }
\/=     { return SLASHEQ; }
\.      { return '.'; }
\.\.    { return DOTDOT; }
\.\.\.  { return DOTDOTDOT; }
&       { return '&'; }
&=      { return ANDEQ; }
&&      { return ANDAND; }
\|      { return '|'; }
\|=     { return OREQ; }
\|\|    { return OROR; }
\-      { return '-'; }
\-=     { return MINUSEQ; }
\-\-    { return DECREMENT; }
\+      { return '+'; }
\+=     { return PLUSEQ; }
\+\+    { return INCREMENT; }
\<      { return '<'; }
\<=     { return LE; }
\<\<    { return LSH; }
\<\<=   { return LSHEQ; }
\>      { return '>'; }
\>=     { return RE; }
\>\>=   { return RSHEQ; }
\>\>\>= { return RSHRSHEQ; }
\>\>    { return RSH; }
\>\>\>  { return RSHRSH; }
\!      { return '!'; }
\!=     { return NOTEQ; }
\(      { return '('; }
\)      { return ')'; }
\[      { return '['; }
\]      { return ']'; }
\{      { return '{'; }
\}      { return '}'; }
\?      { return '?'; }
,       { return ','; }
;       { return ';'; }
:       { return ':'; }
\$      { return '$'; }
=       { return '='; }
==      { return EQEQ; }
\*      { return '*'; }
\*=     { return STAREQ; }
%       { return '%'; }
%=      { return PERCENTEQ; }
\^      { return '^'; }
\^=     { return CARETEQ; }
\^\^    { return CARETCARET; }
\^\^=   { return CARETCARETEQ; }
\~      { return '~'; }
\~=     { return TILDEEQ; }

=\> { return EQRSH; /* For lambda expression. */ }

<<EOF>> { return 0; }

%%
