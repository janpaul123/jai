#include "symbol.h"
#include "lexer.h"

int symtable::GetIndex(std::string Name) {
  for (int i = 0; i < symbols.size(); ++i) {
    if (symbols[i].Name.compare(Name) == 0) {
      return i;
    }
  }

  return 0;
}

symtable_entry *symtable::Insert(std::string Name, int Type) {
  if (GetIndex(Name) == 0) {
    symbols.push_back((symtable_entry){Name, Type});
  }

  return Lookup(Name);
}

symtable_entry *symtable::Lookup(std::string Name) {
  return &symbols[GetIndex(Name)];
}

symtable_entry *symtable::FindFirstOfType(int T) {
  for (int i = 0; i < symbols.size(); ++i) {
    if (symbols[i].SymbolType == T) {
      return &symbols[i];
    }
  }

  return &symbols[0];
}

void symtable::OpenScope() { StackedTables.push_back(*this); }

void symtable::CloseScope() {
  *this = StackedTables.back();
  StackedTables.pop_back();
}

symtable::symtable() {
  symbols.reserve(256);
  Insert("", 0);
  Insert("const", token::CONST);
  Insert("bool", token::BOOL);
  Insert("float", token::FLOAT);
  Insert("char", token::CHAR);
  Insert("int", token::INT);
  Insert("break", token::BREAK);
  Insert("continue", token::CONTINUE);
  Insert("do", token::DO);
  Insert("else", token::ELSE);
  Insert("for", token::FOR);
  Insert("if", token::IF);
  Insert("return", token::RETURN);
  Insert("new", token::NEW);
  Insert("delete", token::DELETE);
  Insert("defer", token::DEFER);
}
