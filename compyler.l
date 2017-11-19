%{
    #include <stdlib.h>
    #include <string.h>
    #include "AST.h"
    #include "y.tab.h"

%}

NoZeroDecimal   [1-9][0-9]*
Decimal         [0-9]
Alpha           [a-zA-Z_]
Hexa            [a-fA-F0-9]
Octal           [0-7]

%%

"bool"      { return(BOOL); }
"break"     { return(BREAK); }
"char"      { return(CHAR); }
"const"     { return(CONST); }
"continue"  { return(CONTINUE); }
"else"      { return(ELSE); }
"extern"    { return(EXTERN); }
"float4"    { return(FLOAT4); }
"float8"    { return(FLOAT8); }
"float"     { return(FLOAT4); }
"for"       { return(FOR); }
"if"        { return(IF); }
"in"        { return(IN); }
"int2"      { return(INT2); }
"int4"      { return(INT4); }
"int8"      { return(INT8); }
"int"       { return(INT4); }
"printf"    { return(PRINT); }
"return"    { return(RETURN); }
"scanf"     { return(SCAN); }
"sizeof"    { return(SIZEOF); }
"static"    { return(STATIC); }
"void"      { return(VOID); }
"volatile"  { return(VOLATILE); }
"while"     { return(WHILE); }
"include"   { return(INCLUDE); }
"define"    { return(DEFINE); }
"ifdef"     { return(IFDEF); }
"ifndef"    { return(IFNDEF); }
"endif"     { return(ENDIF); } 



\"(\\.|[^\\\"])*\"              { yylval.sval = strdup(yytext);  return(STRING_LITERAL);
                                    /* either \. comes in sequence
                                    or any characters besides \ and ". */ }

{Alpha}({Alpha}|{Decimal})*     { yylval.sval = yytext; return(IDENTIFIER);
                                    /* if check_type() not needed, just return(IDENTIFIER) */ }


{NoZeroDecimal}                 { yylval.ival = atoi(yytext); return(CONSTANT_INT);
                                    /* decimal expression starting with none-zero */ }

{Decimal}                       { yylval.ival = atoi(yytext); return(CONSTANT_INT); }

{NoZeroDecimal}"."{Decimal}*    { yylval.fval = atof(yytext); return(CONSTANT_FLOAT);
                                    /* floating point ( there is a number before . ) */}
"."{Decimal}+                   { yylval.fval = atof(yytext); return(CONSTANT_FLOAT);
                                    /* floating point ( with no number before . ) */}

\'[a-zA-Z0-9]\'               { yylval.sval = yytext; return(CONSTANT_CHAR);
                                    /* includes multi character literal */ }


"="         { return('='); }
"+="        { return(ADD_ASSIGN); }
"-="        { return(SUB_ASSIGN); }
"*="        { return(MUL_ASSIGN); }
"/="        { return(DIV_ASSIGN); }
"%="        { return(MOD_ASSIGN); }

"++"        { return(INC_OP); }
"--"        { return(DEC_OP); }

"&&"        { return(AND_OP); }
"||"        { return(OR_OP); }

"<="        { return(LE_OP); }
">="        { return(GE_OP); }
"=="        { return(EQ_OP); }
"!="        { return(NE_OP); }
">"         { return('>'); }
"<"         { return('<'); }

";"         { return(';'); }
("{"|"<%")  { return('{'); }
("}"|"%>")  { return('}'); }
","         { return(','); }
":"         { return(':'); }
"("         { return('('); }
")"         { return(')'); }
("["|"<:")  { return('['); }
("]"|":>")  { return(']'); }
"!"         { return('!'); }

"-"         { return('-'); }
"+"         { return('+'); }
"*"         { return('*'); }
"/"         { return('/'); }
"%"         { return('%'); }

"#"         { return('#'); }

[ \t\v\n\f] { /* skip */ }
.           { /* ignore unexpected characters */ }


%%

int yywrap() {
    return 1;
}
