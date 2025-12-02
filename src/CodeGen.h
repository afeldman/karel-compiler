#ifndef CODEGEN_H
#define CODEGEN_H

// BNFC C backend types - include without extern "C" to get enum visibility
#include "Absyn.h"

#include <string>
#include <map>
#include <vector>

class CodeGenerator {
public:
    CodeGenerator();
    
    // Generate C code from Karel AST (C struct)
    std::string generateC(Karel ast);
    
private:
    int indent_level;
    std::map<std::string, std::string> var_types;
    
    std::string indent();
    void increaseIndent();
    void decreaseIndent();
    
    // Code generation for different AST nodes
    std::string genProgram(Karel ast);
    std::string genDeclBlock(DeclBlock decl);
    std::string genVarDecl(VarDecl var);
    std::string genRoutineDefinition(RoutineDefinitionBlock routine);
    std::string genStatement(Stm stmt);
    std::string genExpression(Expression expr);
    
    // Type mapping
    std::string mapType(DataTypes type);
    std::string mapType(ParameterDataTypes type);
    
    // Built-in runtime functions
    std::string getIncludesAndDefines();
};

#endif // CODEGEN_H
