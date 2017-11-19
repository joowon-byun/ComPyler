#include <cstring>
using namespace std;

class Symbol{
public:
	int offset, size;
	char* name;
	Symbol *nextSymbol;
};

class ListSymbol:public Symbol{

};

class ScalarSymbol:public Symbol{

};

class FunctionSymbol:public Symbol{

};

class SymbolTable{
public:
	int BasePointer, StackPointer;
	SymbolTable* prevScope;
	Symbol *head, *end;

	SymbolTable(SymbolTable* _prevScope, int prevSP) : prevScope(_prevScope), head(NULL), end(NULL), BasePointer(prevSP), StackPointer(prevSP) {};

	void SymbolEntry(Symbol* newSymbol){
		if(end == NULL){
			head = newSymbol;
			end = head;
			StackPointer += newSymbol->size;
		}else{
			end->nextSymbol = newSymbol;
			end = newSymbol;
			StackPointer += newSymbol->size;
		}
	}

	int CodeAddress(char* name){
		Symbol *tempSymbol = head;

		while(tempSymbol != NULL){
			if(!strcmp(tempSymbol->name, name)){
				return tempSymbol->address;
			}

			tempSymbol->nextSymbol;
		}
	}
};

class SymbolScope{
	SymbolTable* currentScope;

public:
	SymbolScope(){
		currentScope = new SymbolTable();
	}

	void EnterScope(){
		SymbolTable* newScope = new SymbolTable(currentScope, currentScope->StackPointer);
		currentScope = newScope;
	}

	void ExitScope(){
		SymbolTable* tempScope = currentScope;
		currentScope = currentScope->prevScope;
		free(tempScope);
	}

	int CodeL(char* name){
		SymbolTable* tempScope = currentScope;

		while(tempScope != NULL){
			int address = tempScope->CodeAddress(name);
			if(address != -1){	
				return address;
			}

			tempScope = tempScope->prevScope;
		}

		return NULL;
	}

	int CodeR(char* name){
		SymbolTable* tempScope = currentScope;

		while(tempScope != NULL){
			int address = tempScope->CodeAddress(name);
			if(address != -1){	
				return address;
			}

			tempScope = tempScope->prevScope;
		}

		return NULL;
	}
};