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
    symbols.push_back((symtable_entry){Name, Type, nullptr});
  }

  return Lookup(Name);
}

symtable_entry *symtable::Lookup(std::string Name) {
  int index = GetIndex(Name);
  if (!index) return nullptr;
  return &symbols[index];
}

symtable_entry *symtable::FindFirstOfType(int T) {
  for (int i = 0; i < symbols.size(); ++i) {
    if (symbols[i].SymbolType == T) {
      return &symbols[i];
    }
  }

  return nullptr;
}

symtable::symtable() {
  symbols.reserve(256);
  Insert("", 0);
}
