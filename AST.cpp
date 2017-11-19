#include "AST.h"
#include <cstring>
#include <cstdio>

const int INDENT_LEVEL = 4;

void indent(int n)
{
    if (n > 0) {
        printf(" ");
        indent(--n);
    }
}

Program::Program(PreProcessor* _pre, TranslationUnit* _translation) {
	pre = _pre;
	translation = _translation;
}

void Program::Traversal() {
	if (pre != NULL)
		pre->Traversal();

	if (translation != NULL)
		translation->Traversal();
}

void Program::printTreeIndent(int lmargin) {
    indent(lmargin);
    printf("Start Program\n");
    if (pre != NULL)
      pre->printTreeIndent(lmargin+INDENT_LEVEL);
    if (translation != NULL)
      translation->printTreeIndent(lmargin+INDENT_LEVEL);
}

TranslationUnit::TranslationUnit(){
	nextTranslationUnit = NULL;
}

void TranslationUnit::InsertTranslationUnit(TranslationUnit* _nextTranslationUnit) {
	TranslationUnit* temp = nextTranslationUnit;
	if(temp == NULL)
		nextTranslationUnit = _nextTranslationUnit;
	else{
		while (temp->nextTranslationUnit != NULL)
			temp = temp->nextTranslationUnit;
		temp->nextTranslationUnit = _nextTranslationUnit;
	}
}

void TranslationUnit::Traversal() {
	if (nextTranslationUnit != NULL)
		nextTranslationUnit->Traversal();
}

void TranslationUnit::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Translation Unit\n");
    
  if (nextTranslationUnit != NULL) {
    indent(lmargin);
    printf("Next Translation Unit\n");
    nextTranslationUnit->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

FunctionTranslation::FunctionTranslation(FunctionDeclaration* _func) : TranslationUnit::TranslationUnit() {
	func = _func;
}

void FunctionTranslation::Traversal() {
	func->Traversal();
	TranslationUnit::Traversal();
}

void FunctionTranslation::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Function Translation\n");
  
  func->printTreeIndent(lmargin+INDENT_LEVEL);
  TranslationUnit::printTreeIndent(lmargin+INDENT_LEVEL);
}

DeclarationTranslation::DeclarationTranslation(Declaration* _decl) : TranslationUnit::TranslationUnit() {
	decl = _decl;
}

void DeclarationTranslation::Traversal() {
	decl->Traversal();
	TranslationUnit::Traversal();
}

void DeclarationTranslation::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Declaration Translation\n");
  decl->printTreeIndent(lmargin+INDENT_LEVEL);
  TranslationUnit::printTreeIndent(lmargin+INDENT_LEVEL);
}

FunctionDeclaration::FunctionDeclaration(Type* _type, Declarator* _parameter, Statement* _implementation){
	returnType = _type;
	parameter = _parameter;
	implementation = _implementation;
}

void FunctionDeclaration::Traversal() {
	if (parameter != NULL)
		parameter->Traversal();

	if (implementation != NULL)
		implementation->Traversal();
}

void FunctionDeclaration::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Function Declaration\n");
  
  if (parameter != NULL) {
    indent(lmargin);
    printf("Parameter\n");
    parameter->printTreeIndent(lmargin+INDENT_LEVEL);
  }
  if (implementation != NULL) {
    indent(lmargin);
    printf("Implementation\n");
    implementation->printTreeIndent(lmargin+INDENT_LEVEL);
  }
  
}

ScalarType::ScalarType(char* _typeName, int _size) {
	typeName = strdup(_typeName);
	size = _size;
}

void ScalarType::Traversal() {
  //printf("%s\n", typeName);
}

void ScalarType::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Scalar Type (%s)\n", typeName);
}

ListType::ListType(Type* _type, int _start, int _end) {
	type = _type;
	start = _start;
	end = _end;
}

void ListType::Traversal() {
	type->Traversal();
	//printf("%d TO %d\n", start, end);
}

void ListType::printTreeIndent(int lmargin) {
  type->printTreeIndent(lmargin+INDENT_LEVEL);

  indent(lmargin);
  printf("List Type (start : %d, end : %d)\n", start, end);
}

TupleType::TupleType(TupleType* _type, Type* _nextType) {
	type = _type;
	nextType = _nextType;
}

void TupleType::Traversal() {
	nextType->Traversal();
        if(type != NULL)
        	type->Traversal();
}

void TupleType::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Tuple Type\n");
  
  nextType->printTreeIndent(lmargin+INDENT_LEVEL);
  if (type != NULL)
    type->printTreeIndent(lmargin+INDENT_LEVEL);
}

PreProcessor::PreProcessor(){
	nextPreProcessor = NULL;
}

void PreProcessor::InsertPreProcessor(PreProcessor* _nextPreProcessor) {
	PreProcessor* temp = nextPreProcessor;

	if(temp == NULL){
		nextPreProcessor = _nextPreProcessor;
	}else
	{
		while (temp->nextPreProcessor != NULL)
			temp = temp->nextPreProcessor;
		temp->nextPreProcessor = _nextPreProcessor;
	}
}

void PreProcessor::Traversal() {

	if (nextPreProcessor != NULL){
		nextPreProcessor->Traversal();
	}
}

void PreProcessor::printTreeIndent(int lmargin) {
    indent(lmargin);
    printf("Preprocessor\n");
    
    if (nextPreProcessor != NULL){
      nextPreProcessor->printTreeIndent(lmargin+INDENT_LEVEL);
    }
}

Include::Include(bool _isInCurDir, char* _headerFile) : PreProcessor::PreProcessor() {
	Include::PreProcessor();
	isInCurDirectory = _isInCurDir;
	headerFile = strdup(_headerFile);
}

void Include::Traversal() {
	if (isInCurDirectory) {
	  //printf("INCLUDE \"%s\"\n", headerFile);
	}
	else {
	  //printf("INCLUDE <%s>\n", headerFile);
	}

	PreProcessor::Traversal();
}

void Include::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Include\n");
  
  if (isInCurDirectory) {
	  printf("INCLUDE \"%s\"\n", headerFile);
  }
  else {
	  printf("INCLUDE <%s>\n", headerFile);
  }
  
  PreProcessor::printTreeIndent(lmargin);
}

Define::Define(char* _defineName, Exp* _definedExpression) : PreProcessor::PreProcessor(){
	DefineName = strdup(_defineName);
	DefinedExpression = _definedExpression;
}

void Define::Traversal() {
  //printf("%s\n", DefineName);
	if (DefinedExpression != NULL)
		DefinedExpression->Traversal();

	PreProcessor::Traversal();
}

void Define::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("DEFINE\n");

  if (DefinedExpression != NULL)
    DefinedExpression->printTreeIndent(lmargin+INDENT_LEVEL);

  PreProcessor::printTreeIndent(lmargin+INDENT_LEVEL);
}

IfDefine::IfDefine(bool _isDefined, char* _conditionName, Declaration* _declareList, Function* _funcDeclare) : PreProcessor::PreProcessor(){
	isDefined = _isDefined;
	conditionName = strdup(_conditionName);
	declareList = _declareList;
	funcDeclare = _funcDeclare;
}

void IfDefine::Traversal() {
	if (isDefined) {
	  //	printf("IFDEF %s\n", conditionName);
	}
	else {
	  //	printf("IFNDEF %s\n", conditionName);
	}

	if (declareList != NULL)
		declareList->Traversal();

	if (funcDeclare != NULL)
		funcDeclare->Traversal();

	PreProcessor::Traversal();
}

void IfDefine::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("IfDefine\n");
    
  if (isDefined) {
    indent(lmargin);
    printf("IFDEF %s\n", conditionName);
  }
  else {
    indent(lmargin);
    printf("IFNDEF %s\n", conditionName);
  }
  
  if (declareList != NULL) {
    indent(lmargin);
    printf("DeclareList\n");
    declareList->printTreeIndent(lmargin+INDENT_LEVEL);
  }
  
  if (funcDeclare != NULL) {
    indent(lmargin);
    printf("Function Declaration\n");
    funcDeclare->printTreeIndent(lmargin+INDENT_LEVEL);
  }
  
  PreProcessor::printTreeIndent(lmargin+INDENT_LEVEL);
}

Declaration::Declaration(Type* _type, Declarator* _decl){
	type = _type;
	decl = _decl;
}

void Declaration::InsertDeclaration(Declaration* _nextDecl){
	Declaration* temp = nextDeclaration;
	if(temp == NULL){
		nextDeclaration = _nextDecl;
	}else{
		while(temp->nextDeclaration != NULL)
			temp = temp->nextDeclaration;

		temp->nextDeclaration = _nextDecl;
	}
}

void Declaration::Traversal(){
	type->Traversal();

	if(decl != NULL)
		decl->Traversal();

	if(nextDeclaration != NULL)
		nextDeclaration->Traversal();
}

void  Declaration::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Declaration\n");
  
  type->printTreeIndent(lmargin+INDENT_LEVEL);

  if(decl != NULL) {
    indent(lmargin);
    printf("Declaration\n");
    decl->printTreeIndent(lmargin+INDENT_LEVEL);
  }

  if(nextDeclaration != NULL) {
    indent(lmargin);
    printf("Next Declaration\n");
    
    nextDeclaration->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

Declarator::Declarator(Declarator* _decl) {
	decl = _decl;
}

void Declarator::Traversal() {
  if (decl != NULL)
    decl->Traversal();
}

void Declarator::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Declarator\n");

  if (decl != NULL) {
    indent(lmargin);
    printf("Declarator\n");
    decl->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

DeclaratorList::DeclaratorList(Declarator* _decl, DeclaratorList* _declList) : Declarator::Declarator(_decl) {
	nextDeclarator = _declList;
}

void DeclaratorList::Traversal(){
	Declarator::Traversal();

	if(nextDeclarator != NULL)
		nextDeclarator->Traversal();
}

void DeclaratorList::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Declarator List\n");
  
  Declarator::printTreeIndent(lmargin+INDENT_LEVEL);

  if(nextDeclarator != NULL) {
    indent(lmargin);
    printf("Next Declarator\n");
    nextDeclarator->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

IDDeclarator::IDDeclarator(Declarator* _decl, const char* _id) : Declarator::Declarator(_decl) {
	id = strdup(_id);
}

void IDDeclarator::Traversal(){
	Declarator::Traversal();

	//printf("%s\n", id);
}

void IDDeclarator::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("ID Declarator\n");
  
  Declarator::printTreeIndent(lmargin+INDENT_LEVEL);
  indent(lmargin);
  printf("ID Declarator (%s)\n", id);
}

InitialDeclarator::InitialDeclarator(Declarator* _decl, ExpList* _initializer) : Declarator::Declarator(_decl) {
	initializer = _initializer;
}

void InitialDeclarator::Traversal(){
	Declarator::Traversal();

	if(initializer != NULL)
		initializer->Traversal();
}

void InitialDeclarator::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Initial Declarator\n");
  
  Declarator::printTreeIndent(lmargin+INDENT_LEVEL);

  if (initializer != NULL) {
    indent(lmargin);
    printf("Initializer\n");
    
    initializer->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

ListDeclarator::ListDeclarator(Declarator* _decl, Exp* _exp) : Declarator::Declarator(_decl) {
	constExpression = _exp;
}

void ListDeclarator::Traversal(){
	Declarator::Traversal();

	if(constExpression != NULL)
		constExpression->Traversal();
}

void ListDeclarator::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("List Declarator\n");
  
  Declarator::printTreeIndent(lmargin+INDENT_LEVEL);

  if(constExpression != NULL) {
    indent(lmargin);
    printf("constant Expression\n");
    constExpression->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

FunctionDeclarator::FunctionDeclarator(Declarator* _decl, Declaration* _parameter) : Declarator::Declarator(_decl) {
	parameter = _parameter;
}

void FunctionDeclarator::Traversal(){
	decl->Traversal();

	if(parameter != NULL)
		parameter->Traversal();
}

void FunctionDeclarator::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Function Declarator\n");
  
  decl->printTreeIndent(lmargin);

  if(parameter != NULL) {
    indent(lmargin);
    printf("Parameter\n");
    parameter->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

void FunctionDeclarator::InsertNextDeclaration(Declaration* _decl){
	Declaration* temp = parameter;
	if(temp == NULL){
		temp = _decl;
	}else{
		while(temp->nextDeclaration != NULL)
			temp = temp->nextDeclaration;
		temp->nextDeclaration = _decl;
	}
}

void Statement::InsertStatement(Statement* _nextStatement){
	Statement* temp = nextStatement;
	if(temp == NULL){
		nextStatement = _nextStatement;
	}else{
		while(temp->nextStatement != NULL)
			temp = temp->nextStatement;

		temp->nextStatement = _nextStatement;
	}
}

Statement::Statement(){
	nextStatement = NULL;
}

void Statement::Traversal() {
	if (nextStatement != NULL)
		nextStatement->Traversal();
}

void Statement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Statement\n");
  
  if (nextStatement != NULL) {
    indent(lmargin);
    printf("Next Statement\n");
  
    nextStatement->printTreeIndent(lmargin+INDENT_LEVEL);
  }
}

DeclarationStatement::DeclarationStatement(Declaration* _decl) {
	decl = _decl;
}

void DeclarationStatement::Traversal() {
	decl->Traversal();
	Statement::Traversal();
}

void DeclarationStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Declaration Statement\n");

  decl->printTreeIndent(lmargin+INDENT_LEVEL);
  Statement::printTreeIndent(lmargin+INDENT_LEVEL);
}

ExpressionStatement::ExpressionStatement(Exp* _expression){
	expression = _expression;
}

void ExpressionStatement::Traversal() {
	if(expression != NULL)
		expression->Traversal();
	Statement::Traversal();
}

void ExpressionStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Expression Statement\n");

  if(expression != NULL) {
    indent(lmargin);
    printf("Expression\n");
    
    expression->printTreeIndent(lmargin+INDENT_LEVEL);
  }
  Statement::printTreeIndent(lmargin+INDENT_LEVEL);
}

ConditionStatement::ConditionStatement(Exp* _condition, Statement* _then, Statement* _else) {
	condition = _condition;
	thenStatement = _then;
	elseStatement = _else;
}

void ConditionStatement::Traversal() {
	printf("IF\n");
	condition->Traversal();

	if (thenStatement != NULL)
		thenStatement->Traversal();

	if (elseStatement != NULL)
		elseStatement->Traversal();

	Statement::Traversal();
}

void ConditionStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Condition Statement\n");

  indent(lmargin);
  printf("IF\n");
  condition->printTreeIndent(lmargin + INDENT_LEVEL);

  if (thenStatement != NULL) {
    indent(lmargin);
    printf("THEN\n");
    thenStatement->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  if (elseStatement != NULL) {
    indent(lmargin);
    printf("ELSE\n");
    elseStatement->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  Statement::printTreeIndent(lmargin + INDENT_LEVEL);
  
}

LoopStatement::LoopStatement(Statement* _thenStatement) {
	thenStatement = _thenStatement;
}

void LoopStatement::Traversal() {
	if (thenStatement != NULL)
		thenStatement->Traversal();

	Statement::Traversal();
}

void LoopStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Loop Statement\n");

  if (thenStatement != NULL) {
    indent(lmargin);
    printf("THEN Statement\n");
    thenStatement->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  Statement::printTreeIndent(lmargin + INDENT_LEVEL);
}

ForStatement::ForStatement(Statement* _then, ExpList* _init, ExpList* _condition, ExpList* _looping) : LoopStatement(_then) {
	init = _init;
	condition = _condition;
	looping = _looping;
}

void ForStatement::Traversal() {
	if (init != NULL)
		init->Traversal();

	if (condition != NULL)
		condition->Traversal();

	if (looping != NULL)
		looping->Traversal();

	LoopStatement::Traversal();
}

void ForStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("For Statement\n");

  if (init != NULL) {
    indent(lmargin);
    printf("Init Expression Statement\n");
    init->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  if (condition != NULL) {
    indent(lmargin);
    printf("Condition Expression Statement\n");
    condition->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  if (looping != NULL) {
    indent(lmargin);
    printf("End Of Loop Expression\n");
    looping->printTreeIndent(lmargin + INDENT_LEVEL);
  }

  LoopStatement::printTreeIndent(lmargin + INDENT_LEVEL);
}

WhileStatement::WhileStatement(Statement* _then, Exp* _condition) : LoopStatement(_then) {
	condition = _condition;
}

void WhileStatement::Traversal() {
	condition->Traversal();
	LoopStatement::Traversal();
}

void WhileStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("While Statement\n");

  condition->printTreeIndent(lmargin + INDENT_LEVEL);
  LoopStatement::printTreeIndent(lmargin + INDENT_LEVEL);
}

ForinStatement::ForinStatement(Statement* _then, Exp* _dest, Exp* _src) : LoopStatement(_then) {
	dest = _dest;
	src = _src;
}

void ForinStatement::Traversal() {
	dest->Traversal();
	src->Traversal();
	LoopStatement::Traversal();
}

void ForinStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("For-In Statement\n");

  dest->printTreeIndent(lmargin + INDENT_LEVEL);
  src->printTreeIndent(lmargin + INDENT_LEVEL);
  LoopStatement::printTreeIndent(lmargin + INDENT_LEVEL);
}

JumpStatement::JumpStatement(char* _controlStatement, Exp* _num){
	controlStatement = strdup(_controlStatement);
	num = _num;
}

void JumpStatement::Traversal(){
  //printf("%s[]\n",controlStatement);
}

void JumpStatement ::printTreeIndent(int lmargin){
  indent(lmargin);
  printf("Jump Statement %s[]\n", controlStatement);
}

IOStatement::IOStatement(bool _isPrint, char* _msgFormat, ExpList* _parameter) {
	isPrint = _isPrint;
	msgFormat = strdup(_msgFormat);
	parameter = _parameter;
}

void IOStatement::Traversal() {
	if (isPrint) {
	  //printf("PRINT(%s)\n", msgFormat);
	}
	else {
	  //printf("SCAN(%s)\n", msgFormat);
	}

	if(parameter != NULL)
		parameter->Traversal();
	Statement::Traversal();
}

void IOStatement::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("IO Statement\n");

  if (isPrint) {
    indent(lmargin);
    printf("PRINT(%s)\n", msgFormat);
  }
  else {
    indent(lmargin);
    printf("SCAN(%s)\n", msgFormat);
  }
  
  if(parameter != NULL)
    parameter->printTreeIndent(lmargin + INDENT_LEVEL);
  Statement::printTreeIndent(lmargin + INDENT_LEVEL)           ;
}

ExpList::ExpList(ExpList* _expression, Exp* _nextExp ) {
	expression = _expression;
	nextExp = _nextExp;
}

void ExpList::Traversal() {
	if(expression != NULL)
		expression->Traversal();

	nextExp->Traversal();
}

void ExpList::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Expression List \n");

  if(expression != NULL)
    expression->printTreeIndent(lmargin + INDENT_LEVEL);
  
  nextExp->printTreeIndent(lmargin + INDENT_LEVEL);
}

BOPExp::BOPExp(char* _op, Exp* _expLeft, Exp* _expRight) {
	op = strdup(_op);
	expLeft = _expLeft;
	expRight = _expRight;
}

void BOPExp::Traversal() {
  //printf("[%s]\n", op);
	expLeft->Traversal();
	expRight->Traversal();
}

void BOPExp::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Binary Operation Expression (%s) \n", op);

  expLeft->printTreeIndent(lmargin + INDENT_LEVEL);
  expRight->printTreeIndent(lmargin + INDENT_LEVEL);

}

UOPExp::UOPExp(bool _isPrefix, char* _op, Exp* _exp) {
	isPrefix = _isPrefix;
	op = strdup(_op);
	exp = _exp;
}

void UOPExp::Traversal() {
	if (isPrefix) {
	  //printf("UOP(%s)\n", op);
		exp->Traversal();
	}
	else {
		exp->Traversal();
		//printf("UOP(%s)\n", op);
	}
}

void UOPExp::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Unary Operation Expression (%s) \n", op);

  if (isPrefix) {
    indent(lmargin);
    printf("UOP(%s)\n", op);
    exp->printTreeIndent(lmargin + INDENT_LEVEL);
  }
  else {
    indent(lmargin);
    exp->printTreeIndent(lmargin + INDENT_LEVEL);
    printf("UOP(%s)\n", op);
  }
}

List::List(Exp* _exp, ExpList* _index) {
	exp = _exp;
	index = _index;
}

void List::Traversal(){
	exp->Traversal();
	//printf("[%d]\n", index);
}

void List::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("List Expression \n");

  exp->printTreeIndent(lmargin + INDENT_LEVEL);
}

Value::Value(char* _sVal, bool isID) {
	if (isID) {
		type = 0;
		val.id = strdup(_sVal);
	}
	else {
		type = 1;
		val.sVal = strdup(_sVal);
	}
}

Value::Value(int _iVal) {
	type = 2;

	val.iVal = _iVal;
}

Value::Value(float _fVal) {
	type = 3;

	val.fVal = _fVal;
}

Value::Value(char _cVal) {
  type = 4;

  val.cVal = _cVal;
}

void Value::Traversal() {
	switch (type) {
	case 0:
	  //printf("ID(%s)\n", val.id);
		break;
	case 1:
	  //printf("STR(%s)\n", val.sVal);
		break;
	case 2:
	  //printf("INT(%d)\n", val.iVal);
		break;
	case 3:
	  //printf("FLOAT(%f)\n", val.fVal);
		break;
	}
}

void Value::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Value Expression \n");


  indent(lmargin);
  switch (type) {
  case 0:
	  printf("ID(%s)\n", val.id);
	  break;
  case 2:
	  printf("INT(%d)\n", val.iVal);
	  break;
  case 3:
	  printf("FLOAT(%f)\n", val.fVal);
	  break;
  default:
          printf("CHAR()\n");
	  break;
  
  }
}

ListComprehension::ListComprehension(Exp* _result, Statement* _statement) {
	result = _result;
	statement = _statement;
}

void ListComprehension::Traversal() {
	result->Traversal();
	statement->Traversal();
}

void ListComprehension::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("List Comprehensive Expression \n");

  result->printTreeIndent(lmargin + INDENT_LEVEL);
  statement->printTreeIndent(lmargin + INDENT_LEVEL);
}

Function::Function(Exp* _exp, ExpList* _parameter) {
	exp = _exp;
	parameter = _parameter;
}

void Function::Traversal() {
	exp->Traversal();

	if (parameter != NULL)
		parameter->Traversal();
}

void Function::printTreeIndent(int lmargin) {
  indent(lmargin);
  printf("Function Expression \n");

  exp->printTreeIndent(lmargin + INDENT_LEVEL);

  if (parameter != NULL)
    parameter->printTreeIndent(lmargin + INDENT_LEVEL);
}
