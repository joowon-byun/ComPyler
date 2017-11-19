#include "AST_ClassDefine.h"
using namespace std;

class Node {
public:
	virtual void Traversal() = 0;
	virtual void printTreeIndent(int lmargin) = 0;
};

class Program : public Node {
protected:
	PreProcessor* pre;
	TranslationUnit* translation;

public:
	Program(PreProcessor*, TranslationUnit*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class TranslationUnit {
protected:
	TranslationUnit* nextTranslationUnit;
public:
	TranslationUnit();
	virtual void Traversal();
	virtual void printTreeIndent(int lmargin);
	void InsertTranslationUnit(TranslationUnit*);
};

class FunctionTranslation : public TranslationUnit {
protected:
	FunctionDeclaration* func;
public:
	FunctionTranslation(FunctionDeclaration*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class DeclarationTranslation : public TranslationUnit {
protected:
	Declaration* decl;
public:
	DeclarationTranslation(Declaration*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class FunctionDeclaration : public Node {
protected:
	Type* returnType;
	Declarator* parameter;
	Statement* implementation;
public:
	FunctionDeclaration(Type*, Declarator*, Statement*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Type : public Node {
protected:
public:
	virtual void Traversal() = 0;
	virtual void printTreeIndent(int lmargin) = 0;
};

class ScalarType : public Type {
protected:
	char* typeName;
	int size;
public:
	ScalarType(char*, int);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ListType : public Type {
protected:
	Type* type;
	int start, end;
public:
	ListType(Type*, int, int);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class TupleType : public Type {
protected:
	TupleType* type;
	Type* nextType;
public:
	TupleType(TupleType*, Type*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class PreProcessor : public Node {
protected:
	PreProcessor* nextPreProcessor;
public:
	PreProcessor();
	void InsertPreProcessor(PreProcessor*);
	virtual void Traversal();
	void printTreeIndent(int lmargin);
};

class Include : public PreProcessor {
protected:
	bool isInCurDirectory;
	char* headerFile;
public:
	Include(bool, char*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Define : public PreProcessor {
protected:
	char* DefineName;
	Exp* DefinedExpression;
public:
	Define(char*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class IfDefine : public PreProcessor {
protected:
	bool isDefined;
	char* conditionName;
	Declaration* declareList;
	Function* funcDeclare;
public:
	IfDefine(bool, char*, Declaration*, Function*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Declaration : public Node {
public:
	Type* type;
	Declarator* decl;
	Declaration* nextDeclaration;
public:
	Declaration(Type*, Declarator*);
	void InsertDeclaration(Declaration*);
	virtual void Traversal();
	virtual void printTreeIndent(int lmargin);
};

class Declarator : public Node {
protected:
	Declarator* decl;
public:
	Declarator(Declarator*);
	virtual void Traversal();
	virtual void printTreeIndent(int lmargin);
	 
};

class DeclaratorList : public Declarator {
protected:
	DeclaratorList* nextDeclarator;
public:
	DeclaratorList(Declarator*, DeclaratorList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class IDDeclarator : public Declarator {
protected:
	char* id;
public:
	IDDeclarator(Declarator*, const char*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class InitialDeclarator : public Declarator{
protected:
	ExpList* initializer;
public:
	InitialDeclarator(Declarator*, ExpList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ListDeclarator : public Declarator {
protected:
	Exp* constExpression;
public:
	ListDeclarator(Declarator*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class FunctionDeclarator : public Declarator {
protected:
	Declaration* parameter;
public:
	FunctionDeclarator(Declarator*, Declaration*);
	void Traversal();
	void printTreeIndent(int lmargin);
	void InsertNextDeclaration(Declaration*);
};

class Statement : public Node {	
public:
        Statement* nextStatement;
	Statement();
	void InsertStatement(Statement*);
	virtual void Traversal();
	virtual void printTreeIndent(int lmargin);
};

class DeclarationStatement : public Statement {
protected:
	Declaration* decl;
public:
	DeclarationStatement(Declaration*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ExpressionStatement : public Statement {
protected:
	Exp* expression;
public:
	ExpressionStatement(Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ConditionStatement : public Statement {
protected:
	Exp* condition;
	Statement *thenStatement, *elseStatement;
public:
	ConditionStatement(Exp*, Statement*, Statement*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class LoopStatement : public Statement {
protected:
	Statement* thenStatement;
public:
	LoopStatement(Statement*);
	virtual void Traversal();
	virtual void printTreeIndent(int lmargin);
};

class ForStatement : public LoopStatement {
protected:
	ExpList *looping, *init, *condition;
public:
	ForStatement(Statement*, ExpList*, ExpList*, ExpList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class WhileStatement : public LoopStatement {
protected:
	Exp *condition;
public:
	WhileStatement(Statement*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ForinStatement : public LoopStatement {
protected:
	Exp *dest, *src;
public:
	ForinStatement(Statement*, Exp*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class JumpStatement : public Statement {
protected:
	char* controlStatement;
	Exp* num;
public:
	JumpStatement(char*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class IOStatement : public Statement {
protected:
	bool isPrint;
	char* msgFormat;
	ExpList* parameter;
public:
	IOStatement(bool, char*, ExpList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Exp : public Node {
public:
	virtual void Traversal() = 0;
};

class ExpList : public Exp {
protected:
    ExpList* expression;
	Exp* nextExp;	
public:
	ExpList(ExpList*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class BOPExp : public Exp {
protected:
	char* op;
	Exp* expLeft, *expRight;
public:
	BOPExp(char*, Exp*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class UOPExp : public Exp {
protected:
	bool isPrefix;
	char* op;
	Exp* exp;
public:
	UOPExp(bool, char*, Exp*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class List : public Exp {
protected:
	Exp* exp;
	ExpList* index;
public:
	List(Exp*, ExpList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Value : public Exp {
protected:
	int type;
	union {
		char* id;
		int iVal;
		float fVal;
	        char cVal;
		char* sVal;
	}val;
public:
	Value(char*, bool);
	Value(int);
	Value(float);
	Value(char);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class ListComprehension : public Exp {
protected:
	Exp* result;
	Statement *statement;
public:
	ListComprehension(Exp*, Statement*);
	void Traversal();
	void printTreeIndent(int lmargin);
};

class Function : public Exp {
protected:
	Exp* exp;
	ExpList* parameter;
public:
	Function(Exp*, ExpList*);
	void Traversal();
	void printTreeIndent(int lmargin);
};
