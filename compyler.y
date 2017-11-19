%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "AST.h"

  Node* Root;

  int yylex(void);
  int yyerror(char *msg);
%}

%union{
	char *sval;
	int ival;
	float fval;
	Node* nodeVal;
	TranslationUnit* translationUnitVal;
	PreProcessor* preprocessorVal;
	FunctionDeclaration* functionDeclVal;
	Statement* statementVal;
	Declaration* declarationVal;
	Declarator* declaratorVal;
	Exp* expressionVal;
	ExpList* expressionListVal;
	Type* typeVal;
	TupleType* tupleTypeVal;
	DeclaratorList* declListVal;
}

%token <sval> IDENTIFIER CONSTANT_CHAR STRING_LITERAL SIZEOF
%token INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN

%token <ival> CONSTANT_INT
%token <ival> BOOL CHAR INT2 INT4 INT8 FLOAT4 FLOAT8 CONST VOLATILE VOID

%token <fval> CONSTANT_FLOAT

%token EXTERN STATIC
%token IF ELSE WHILE FOR IN CONTINUE BREAK

%token MAIN PRINT SCAN RETURN
%token INCLUDE DEFINE IFDEF IFNDEF ENDIF

%type <sval> unary_operator assignment_operator

%type <nodeVal> start_program
%type <translationUnitVal> external_declaration translation_unit
%type <preprocessorVal> preprocessor_list preprocessor
%type <functionDeclVal> function_definition
%type <statementVal> statement compound_statement statement_list expression_statement selection_statement iteration_statement jump_statement io_statement list_comprehensive_expression_list
%type <declarationVal> declaration declaration_list parameter_declaration parameter_type_list 
%type <declaratorVal> declarator init_declarator 
%type <expressionVal> primary_expression postfix_expression unary_expression multiplicative_expression additive_expression relational_expression equality_expression logical_and_expression logical_or_expression list_comprehensive_expression assignment_expression
%type <expressionListVal> expression argument_expression_list initializer_list initializer
%type <tupleTypeVal> tuple_type_specifier
%type <typeVal> scalar_type_specifier list_type_specifier type_specifier declaration_specifiers
%type <declListVal> init_declarator_list

%start start_program

%%

primary_expression
: IDENTIFIER          { $$ = new Value($1, 1); }
| CONSTANT_INT        { $$ = new Value($1); }
| CONSTANT_FLOAT      { $$ = new Value($1); }
| CONSTANT_CHAR       { $$ = new Value($1[1]); }
| STRING_LITERAL      { $$ = new Value($1, 0); }
| '(' expression ')'  { $$ = $2; }
;

postfix_expression
: primary_expression                            { $$ = $1; }
| postfix_expression '[' expression ']'         { $$ = new List( $1, $3); }
| postfix_expression '(' ')'                    { $$ = new Function( $1, NULL); }
| postfix_expression '(' argument_expression_list ')'  { $$ = new Function( $1, $3); }
| postfix_expression INC_OP                     { $$ = new UOPExp( 0, (char*) "++", $1); }
| postfix_expression DEC_OP                     { $$ = new UOPExp( 0, (char*) "--", $1); }
;

argument_expression_list
: assignment_expression                                  { $$ = new ExpList( NULL, $1); }
| argument_expression_list ',' assignment_expression     { $$ = new ExpList( $1, $3); }
;

unary_expression
: postfix_expression                { $$ = $1; }
| INC_OP unary_expression           { $$ = new UOPExp( 1, (char*) "++", $2); }
| DEC_OP unary_expression           { $$ = new UOPExp( 1, (char*) "--", $2); }
| unary_operator unary_expression   { $$ = new UOPExp( 1, $1, $2); }
| SIZEOF unary_expression           { $$ = new UOPExp( 1, (char*) "SIZEOF", $2); }
;

unary_operator
: '&'         { $$ = (char*) "&"; }
| '*'         { $$ = (char*) "*"; }
| '+'         { $$ = (char*) "+"; }
| '-'         { $$ = (char*) "-"; }
| '!'         { $$ = (char*) "!"; }
;

multiplicative_expression
: unary_expression                                  { $$ = $1; }
| multiplicative_expression '*' unary_expression    { $$ = new BOPExp( (char*) "*", $1, $3); }
| multiplicative_expression '/' unary_expression    { $$ = new BOPExp( (char*) "/", $1, $3); }
| multiplicative_expression '%' unary_expression    { $$ = new BOPExp( (char*) "%", $1, $3); }
;

additive_expression
: multiplicative_expression                          { $$ = $1; }
| additive_expression '+' multiplicative_expression  { $$ = new BOPExp( (char*) "+", $1, $3); }
| additive_expression '-' multiplicative_expression  { $$ = new BOPExp( (char*) "-", $1, $3); }
;

relational_expression
: additive_expression                               { $$ = $1; }
| relational_expression '<' additive_expression     { $$ = new BOPExp( (char*) "<", $1, $3); }
| relational_expression '>' additive_expression     { $$ = new BOPExp( (char*) ">", $1, $3); }
| relational_expression LE_OP additive_expression   { $$ = new BOPExp( (char*) "<=", $1, $3); }
| relational_expression GE_OP additive_expression   { $$ = new BOPExp( (char*) ">=", $1, $3); }
;

equality_expression
: relational_expression                             { $$ = $1; }
| equality_expression EQ_OP relational_expression   { $$ = new BOPExp( (char*) "==", $1, $3); }
| equality_expression NE_OP relational_expression   { $$ = new BOPExp( (char*) "!=", $1, $3); }
;

logical_and_expression
: equality_expression                                { $$ = $1; }
| logical_and_expression AND_OP equality_expression  { $$ = new BOPExp( (char*) "&&", $1, $3); }
;

logical_or_expression
: logical_and_expression                                { $$ = $1; }
| logical_or_expression OR_OP logical_and_expression    { $$ = new BOPExp( (char*) "||", $1, $3); }
;

list_comprehensive_expression
: logical_or_expression          { $$ = $1; }
| '[' logical_or_expression '|' list_comprehensive_expression_list ']' { $$ = $2; }
;

list_comprehensive_expression_list
: iteration_statement                                           { $$ = $1; }
| iteration_statement ',' list_comprehensive_expression_list    { $1->InsertStatement($3); $$ = $1; }
| logical_or_expression                                         { $$ = new ExpressionStatement($1); }
| logical_or_expression ',' list_comprehensive_expression_list  { $$ = new ExpressionStatement($1); $$->InsertStatement($3); }
;
       

assignment_expression
: list_comprehensive_expression                              { $$ = $1; }
| unary_expression assignment_operator assignment_expression { $$ = new BOPExp( $2, $1, $3); }
;

assignment_operator
: '='            { $$ = (char*) "=";  }
| MUL_ASSIGN     { $$ = (char*) "*="; }
| DIV_ASSIGN     { $$ = (char*) "/="; }
| MOD_ASSIGN     { $$ = (char*) "%="; }
| ADD_ASSIGN     { $$ = (char*) "+="; }
| SUB_ASSIGN     { $$ = (char*) "-="; }
;

expression
: assignment_expression			 { $$ = new ExpList ( NULL, $1);}
| expression ',' assignment_expression   { $$ = new ExpList( $1, $3); }
;


declaration
: declaration_specifiers ';'			{ $$ = new Declaration($1, NULL); }
| declaration_specifiers init_declarator_list ';'	{ $$ = new Declaration($1, $2); }
;

declaration_specifiers
	: type_specifier					{ $$ = $1; }
	;

init_declarator_list
	: init_declarator  { $$ = new DeclaratorList($1, NULL); }
	| init_declarator_list ',' init_declarator { $$ = new DeclaratorList($3, $1); }
	;

init_declarator
	: declarator  					{ $$ = $1; }
	| declarator '=' initializer  	{ $$ = new InitialDeclarator($1, $3);}
	;

type_specifier
	: scalar_type_specifier  { $$ = $1; }
	| list_type_specifier  { $$ = $1; }
	| '(' tuple_type_specifier ')' { $$ = $2; }
	;

tuple_type_specifier
	: tuple_type_specifier ',' scalar_type_specifier  	{ $$ = new TupleType($1, $3);}
	| scalar_type_specifier		{ $$ = new TupleType(NULL, $1); }
	;

list_type_specifier
: scalar_type_specifier '[' CONSTANT_INT ':' CONSTANT_INT ']'  { $$ = new ListType($1, $3, $5); }
| scalar_type_specifier '[' ']'                                { $$ = new ListType($1,(int) NULL,(int) NULL); }
| list_type_specifier '[' CONSTANT_INT ':' CONSTANT_INT ']'    { $$ = new ListType($1, $3, $5); }
| list_type_specifier '[' ']'                                  { $$ = new ListType($1,(int) NULL,(int) NULL); }
	;

scalar_type_specifier
	: VOID 		{ $$ = new ScalarType( (char*) "void", 0); }
	| BOOL 		{ $$ = new ScalarType( (char*) "bool", 1); }
	| CHAR  	{ $$ = new ScalarType( (char*) "char", 1); }
	| INT2  	{ $$ = new ScalarType( (char*) "int", 2); }
	| INT4  	{ $$ = new ScalarType( (char*) "int", 4); }
	| INT8 		{ $$ = new ScalarType( (char*) "int", 8); }
	| FLOAT4        { $$ = new ScalarType( (char*) "float", 4); }
	| FLOAT8        { $$ = new ScalarType( (char*) "float", 8); }
	;


declarator
	: IDENTIFIER 		{ $$ = new IDDeclarator(NULL, $1); }
	| '(' declarator ')'  	{ $$ = $2; }
	| declarator '(' parameter_type_list ')'  		{ $$ = new FunctionDeclarator($1, $3); }
	| declarator '(' ')'	{ $$ = new FunctionDeclarator($1, NULL); }
	;

parameter_type_list
	: parameter_declaration  		{ $$ = $1; }
	| parameter_type_list ',' parameter_declaration  		{ $1->InsertDeclaration($3); $$ = $1; }
	;

parameter_declaration
	: declaration_specifiers declarator  	{ $$ = new Declaration($1, $2); }
	| declaration_specifiers 				{ $$ = new Declaration($1, NULL); }
	;

initializer
	: assignment_expression  { $$ = new ExpList(NULL, $1); }
	| '{' initializer_list '}' { $$ = $2; }
	| '{' initializer_list ',' '}'   { $$ = $2; }
	;

initializer_list
: initializer 		{ $$ = $1; }
| initializer_list ',' initializer  { $$ = new ExpList($1, $3); }
;

statement
: compound_statement       { $$ = $1; }
| expression_statement     { $$ = $1; }
| selection_statement      { $$ = $1; }
| iteration_statement      { $$ = $1; }
| jump_statement           { $$ = $1; }
| io_statement             { $$ = $1; }
;

compound_statement
: '{' '}'                                  { $$ = NULL; }
| '{' statement_list '}'                   { $$ = $2; }
| '{' declaration_list '}'                 { $$ = new DeclarationStatement( $2); }
| '{' declaration_list statement_list '}'  { $$ = new DeclarationStatement( $2); }
;

declaration_list
: declaration                     { $$ = $1; }
| declaration_list declaration    { $1->InsertDeclaration($2); $$ = $1;}
;

statement_list
: statement                 { $$ = $1; }
| statement_list statement  { $1->InsertStatement($2); $$ = $1;}
;

expression_statement
: ';'               { $$ = new ExpressionStatement( NULL); }
| expression ';'   { $$ = new ExpressionStatement( $1 ); }
;

selection_statement
: IF '(' expression ')' statement                { $$ = new ConditionStatement( $3, $5, NULL); }
| IF '(' expression ')' statement ELSE statement { $$ = new ConditionStatement( $3, $5, $7); }
;

iteration_statement
: WHILE '(' expression ')' statement       { $$ = new WhileStatement(  $5, $3); }
| FOR '(' expression ';' expression ';' ')' statement            { $$ = new ForStatement( $8, $3, $5, NULL); }
| FOR '(' expression ';' expression ';' expression ')' statement { $$ = new ForStatement( $9, $3, $5, $7); }
| FOR expression IN expression statement   { $$ = new ForinStatement( $5, $2, $4);}
;

jump_statement
: CONTINUE ';'               { $$ = new JumpStatement( (char*) "CONTINUE", NULL ); }
| CONTINUE CONSTANT_INT ';'      { Value* tempVal = new Value($2); $$ = new JumpStatement( (char*) "CONTINUE",tempVal ); }
| BREAK ';'                  { $$ = new JumpStatement( (char*) "BREAK", NULL ); }
| BREAK CONSTANT_INT ';'         { Value* tempVal = new Value($2);  $$ = new JumpStatement( (char*) "BREAK", tempVal ); }
| RETURN ';'                 { $$ = new JumpStatement( (char*) "RETURN", NULL ); }
| RETURN expression ';'        { $$ = new JumpStatement( (char*) "RETURN", $2 ); }
;

io_statement
: PRINT '(' STRING_LITERAL ')' ';'           { $$ = new IOStatement( 1, $3, NULL); }
| PRINT '(' STRING_LITERAL ',' argument_expression_list ')' ';' { $$ = new IOStatement( 1, $3, $5); }
| PRINT '(' argument_expression_list ')' ';'  { $$ = new IOStatement( 1, NULL, $3);}
| SCAN  '(' argument_expression_list ')' ';'  { $$ = new IOStatement( 0, NULL, $3);}
;

start_program
        : external_declaration                    { Root = new Program(NULL, $1); }
	| preprocessor_list                       { Root = new Program($1, NULL); }
        | preprocessor_list external_declaration  { Root = new Program($1, $2); }
        ;

preprocessor_list
        : '#' preprocessor                    { $$ = $2; }
        | preprocessor_list '#' preprocessor  { $1->InsertPreProcessor($3); $$ = $1; }
        ;

preprocessor
: INCLUDE STRING_LITERAL                    { $$ = new Include(true, $2);}
| DEFINE IDENTIFIER IDENTIFIER              { $$ = new Define($2, NULL); }   
| IFDEF IDENTIFIER translation_unit ENDIF   { $$ = new IfDefine(true, $2, NULL, NULL); } 
| IFNDEF IDENTIFIER translation_unit ENDIF  { $$ = new IfDefine(false, $2, NULL, NULL); } 
;

external_declaration
: external_declaration translation_unit     { $1->InsertTranslationUnit($2); $$ = $1; }
| translation_unit                          { $$ = $1;}
;

translation_unit
: function_definition                       { $$ = new FunctionTranslation($1);}
| declaration                               { $$ = new DeclarationTranslation($1);}
;

function_definition
: declaration_specifiers declarator compound_statement  { $$ = new FunctionDeclaration($1, $2, $3);}
;


%%

int main() {
  yyparse();
  Root->Traversal();
  Root->printTreeIndent(0);
  return 0;
}

int yyerror(char *s) {
  printf("Error: %s\n", s);
}
