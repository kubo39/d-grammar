// The MIT License (MIT)
//
// Copyright (c) 2014 Brian Leibig
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "tokens.h"

extern char *yytext;

static char *desugar_num(char*, char*);

void print_token(int token) {
  switch (token) {
  case ';': printf("Semi"); break;
  case ',': printf("Comma"); break;
  case '.': printf("Dot"); break;
  case '(': printf("OpenDelim(Paren)"); break;
  case ')': printf("CloseDelim(Paren)"); break;
  case '{': printf("OpenDelim(Brace)"); break;
  case '}': printf("CloseDelim(Brace)"); break;
  case '[': printf("OpenDelim(Bracket)"); break;
  case ']': printf("CloseDelim(Bracket)"); break;
  case '@': printf("At"); break;
  case '#': printf("Pound"); break;
  case '~': printf("Tilde"); break;
  case ':': printf("Colon"); break;
  case '$': printf("Dollar"); break;
  case '?': printf("Question"); break;
  case '=': printf("Eq"); break;
  case '!': printf("Not"); break;
  case '<': printf("Lt"); break;
  case '>': printf("Gt"); break;
  case '-': printf("BinOp(Minus)"); break;
  case '&': printf("BinOp(And)"); break;
  case '|': printf("BinOp(Or)"); break;
  case '+': printf("BinOp(Plus)"); break;
  case '*': printf("BinOp(Star)"); break;
  case '/': printf("BinOp(Slash)"); break;
  case '^': printf("BinOp(Caret)"); break;
  case '%': printf("BinOp(Percent)"); break;
  case ABSTRACT: printf("Ident(ABSTRACT)"); break;
  case ALIAS: printf("Ident(ALIAS)"); break;
  case ALIGN: printf("Ident(ALIGN)"); break;
  case ASM: printf("Ident(ASM)"); break;
  case ASSERT: printf("Ident(ASSERT)"); break;
  case AUTO: printf("Ident(AUTO)"); break;
  case BODY: printf("Ident(BODY)"); break;
  case BOOL: printf("Ident(BOOL)"); break;
  case BREAK: printf("Ident(BREAK)"); break;
  case BYTE: printf("Ident(BYTE)"); break;
  case CASE: printf("Ident(CASE)"); break;
  case CAST: printf("Ident(CAST)"); break;
  case CATCH: printf("Ident(CATCH)"); break;
  case CDOUBLE: printf("Ident(CDOUBLE)"); break;
  case CENT: printf("Ident(CENT)"); break;
  case CFLOAT: printf("Ident(CFLOAT)"); break;
  case CHAR: printf("Ident(CHAR)"); break;
  case CLASS: printf("Ident(CLASS)"); break;
  case CONST: printf("Ident(CONST)"); break;
  case CONTINUE: printf("Ident(CONTINUE)"); break;
  case CREAL: printf("Ident(CREAL)"); break;
  case DCHAR: printf("Ident(DCHAR)"); break;
  case DEBUG: printf("Ident(DEBUG)"); break;
  case DEFAULT: printf("Ident(DEFAULT)"); break;
  case DELEGATE: printf("Ident(DELEGATE)"); break;
  case DELETE: printf("Ident(DELETE)"); break;
  case DEPRECATED: printf("Ident(DEPRECATED)"); break;
  case DO: printf("Ident(DO)"); break;
  case DOUBLE: printf("Ident(DOUBLE)"); break;
  case ELSE: printf("Ident(ELSE)"); break;
  case ENUM: printf("Ident(ENUM)"); break;
  case EXPORT: printf("Ident(EXPORT)"); break;
  case EXTERN: printf("Ident(EXTERN)"); break;
  case FALSE: printf("Ident(FALSE)"); break;
  case FINAL: printf("Ident(FINAL)"); break;
  case FINALLY: printf("Ident(FINALLY)"); break;
  case FLOAT: printf("Ident(FLOAT)"); break;
  case FOR: printf("Ident(FOR)"); break;
  case FOREACH: printf("Ident(FOREACH)"); break;
  case FOREACHREVERSE: printf("Ident(FOREACHREVERSE)"); break;
  case FUNCTION: printf("Ident(FUNCTION)"); break;
  case GOTO: printf("Ident(GOTO)"); break;
  case IDOUBLE: printf("Ident(IDOUBLE)"); break;
  case IF: printf("Ident(IF)"); break;
  case IFLOAT: printf("Ident(IFLOAT)"); break;
  case IMMUTABLE: printf("Ident(IMMUTABLE)"); break;
  case IMPORT: printf("Ident(IMPORT)"); break;
  case IN: printf("Ident(IN)"); break;
  case INOUT: printf("Ident(INOUT)"); break;
  case INT: printf("Ident(INT)"); break;
  case INTERFACE: printf("Ident(INTERFACE)"); break;
  case INVARIANT: printf("Ident(INVARIANT)"); break;
  case IREAL: printf("Ident(IREAL)"); break;
  case IS: printf("Ident(IS)"); break;
  case LAZY: printf("Ident(LAZY)"); break;
  case LONG: printf("Ident(LONG)"); break;
  case MACRO: printf("Ident(MACRO)"); break;
  case MIXIN: printf("Ident(MIXIN)"); break;
  case MODULE: printf("Ident(MODULE)"); break;
  case NEW: printf("Ident(NEW)"); break;
  case NOTHROW: printf("Ident(NOTHROW)"); break;
  case NULLKEYWORD: printf("Ident(NULLKEYWORD)"); break;
  case OUT: printf("Ident(OUT)"); break;
  case OVERRIDE: printf("Ident(OVERRIDE)"); break;
  case PACKAGE: printf("Ident(PACKAGE)"); break;
  case PRAGMA: printf("Ident(PRAGMA)"); break;
  case PRIVATE: printf("Ident(PRIVATE)"); break;
  case PROTECTED: printf("Ident(PROTECTED)"); break;
  case PUBLIC: printf("Ident(PUBLIC)"); break;
  case PURE: printf("Ident(PURE)"); break;
  case REAL: printf("Ident(REAL)"); break;
  case REF: printf("Ident(REF)"); break;
  case RETURN: printf("Ident(RETURN)"); break;
  case SCOPE: printf("Ident(SCOPE)"); break;
  case SHARED: printf("Ident(SHARED)"); break;
  case SHORT: printf("Ident(SHORT)"); break;
  case STATIC: printf("Ident(STATIC)"); break;
  case STRUCT: printf("Ident(STRUCT)"); break;
  case SUPER: printf("Ident(SUPER)"); break;
  case SWITCH: printf("Ident(SWITCH)"); break;
  case SYNCHRONIZED: printf("Ident(SYNCHRONIZED)"); break;
  case TEMPLATE: printf("Ident(TEMPLATE)"); break;
  case THIS: printf("Ident(THIS)"); break;
  case THROW: printf("Ident(THROW)"); break;
  case TRUE: printf("Ident(TRUE)"); break;
  case TRY: printf("Ident(TRY)"); break;
  case TYPEDEF: printf("Ident(TYPEDEF)"); break;
  case TYPEID: printf("Ident(TYPEID)"); break;
  case TYPEOF: printf("Ident(TYPEOF)"); break;
  case UBYTE: printf("Ident(UBYTE)"); break;
  case UCENT: printf("Ident(UCENT)"); break;
  case UINT: printf("Ident(UINT)"); break;
  case ULONG: printf("Ident(ULONG)"); break;
  case UNION: printf("Ident(UNION)"); break;
  case UNITTEST: printf("Ident(UNITTEST)"); break;
  case USHORT: printf("Ident(USHORT)"); break;
  case VERSION: printf("Ident(VERSION)"); break;
  case VOID: printf("Ident(VOID)"); break;
  case VOLATILE: printf("Ident(VOLATILE)"); break;
  case WCHAR: printf("Ident(WCHAR)"); break;
  case WHILE: printf("Ident(WHILE)"); break;
  case WITH: printf("Ident(WITH)"); break;
  case SPFILE: printf("Ident(SPFILE)"); break;
  case SPFILEFULLPATH: printf("Ident(SPFILEFULLPATH)"); break;
  case SPMODULE: printf("Ident(SPMODULE)"); break;
  case SPLINE: printf("Ident(SPLINE)"); break;
  case SPFUNCTION: printf("Ident(SPFUNCTION)"); break;
  case SPPRETTYFUNCION: printf("Ident(SPPRETTYFUNCION)"); break;
  case SPGSHARED: printf("Ident(SPGSHARED)"); break;
  case SPTRAITS: printf("Ident(SPTRAITS)"); break;
  case SPVECTOR: printf("Ident(SPVECTOR)"); break;
  case SPPARAMETERS: printf("Ident(SPPARAMETERS)"); break;
  case IDENTIFIER: printf("IDENTIFIER"); break;

  case INTEGER: printf("INTEGER"); break;
  case DOUBLEQOUTEDSTRING: printf("DoubleQuotedString(%s)", yytext); break;
  case WYSIWYGSTRING: printf("WYSIWYGSTRING(%s)", yytext); break;
  case HEXSTRING: printf("HEXSTRING(%s)", yytext); break;
  case TOKENSTRING: printf("TOKENSTRING(%s)", yytext); break;

  case SLASHEQ: printf("SLASHEQ"); break;
  case DOTDOT: printf("DOTDOT"); break;
  case DOTDOTDOT: printf("DOTDOTDOT"); break;
  case ANDAND: printf("ANDAND"); break;
  case ANDEQ: printf("ANDEQ"); break;
  case OROR: printf("OROR"); break;
  case OREQ: printf("OREQ"); break;
  case MINUSEQ: printf("MINUSEQ"); break;
  case DECREMENT: printf("DECREMENT"); break;
  case PLUSEQ: printf("PLUSEQ"); break;
  case INCREMENT: printf("INCREMENT"); break;
  case LE: printf("LE"); break;
  case LSH: printf("LSH"); break;
  case LSHEQ: printf("LSHEQ"); break;
  case LSHRSH: printf("LSHRSH"); break;
  case LSHRSHEQ: printf("LSHRSHEQ"); break;
  case RE: printf("RE"); break;
  case RSHEQ: printf("RSHEQ"); break;
  case RSHRSHRSHEQ: printf("RSHRSHRSHEQ"); break;
  case RSH: printf("RSH"); break;
  case RSHRSHEQ: printf("RSHRSHEQ"); break;
  case NOTEQ: printf("NOTEQ"); break;
  case NOTLERE: printf("NOTLERE"); break;
  case NOTLEREEQ: printf("NOTLEREEQ"); break;
  case NOTLE: printf("NOTLE"); break;
  case NOTLEEQ: printf("NOTLEEQ"); break;
  case NOTRE: printf("NOTRE"); break;
  case NOTREEQ: printf("NOTREEQ"); break;
  case EQEQ: printf("EQEQ"); break;
  case STAREQ: printf("STAREQ"); break;
  case PERCENTEQ: printf("PERCENTEQ"); break;
  case CARETEQ: printf("CARETEQ"); break;
  case CARETCARET: printf("CARETCARET"); break;
  case CARETCARETEQ: printf("CARETCARETEQ"); break;
  case TILDEEQ: printf("TILDEEQ"); break;
  default: printf("can't print token %d", token); abort();
  }
  printf("\n");
}

static int hex_to_num(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else {
        printf("error: invalid hex digit '%c'\n", c);
        abort();
    }
}

static char *desugar_num(char *tok, char *default_suffix) {
    int len = strlen(tok);
    int start = 0;
    int end;
    long long int val = 0;
    char *res = malloc(64);

    if (tok[0] == '0') {
        start = 2;
    }

    for (int i = 0, k = 0; i < len; i++) {
        if (tok[i] == '_') {
            k = i + 1;
            memmove(tok + i, tok + k, len - i);
            len -= k - i;
        }
    }

    end = len - 1;

    for (int i = len; i > 0; i--) {
        if (tok[i] == 'i' || tok[i] == 'u') {
            end = i;
            break;
        }
    }

    if (tok[1] == 'b') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            if (tok[i] == '1') {
                val += (long long int) pow(2, accum);
            }
        }
    } else if (tok[1] == 'x') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_' || tok[i] == 'u' || tok[i] == 'i') {
                accum--;
                continue;
            }
            val += hex_to_num(tok[i]) * (long long int) pow(16, accum);
        }
    } else if (tok[1] == 'o') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            val += (tok[i] - '0') * (long long int) pow(8, accum);
        }
    } else {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            val += (tok[i] - '0') * (long long int) pow(10, accum);
        }
    }

    if (default_suffix[0] == 'u') {
        snprintf(res, 64, "%llu%s", val, (end == len -1) ? default_suffix : tok + end);
    } else {
        snprintf(res, 64, "%lld%s", val, (end == len -1) ? default_suffix : tok + end);
    }
    return res;
}
