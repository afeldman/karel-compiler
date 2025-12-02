// Include Absyn.h directly to get enum visibility
#include "Absyn.h"
#include "CodeGen.h"
#include <sstream>
#include <string>

CodeGenerator::CodeGenerator() : indent_level(0) {}

std::string CodeGenerator::indent() {
    return std::string(indent_level * 2, ' ');
}

void CodeGenerator::increaseIndent() {
    indent_level++;
}

void CodeGenerator::decreaseIndent() {
    if (indent_level > 0) indent_level--;
}

std::string CodeGenerator::getIncludesAndDefines() {
    return "#include \"runtime.h\"\n"
           "#include <math.h>\n\n";
}

std::string CodeGenerator::generateC(Karel ast) {
    if (!ast) return "";
    
    std::stringstream ss;
    ss << getIncludesAndDefines();
    ss << genProgram(ast);
    return ss.str();
}

std::string CodeGenerator::genProgram(Karel ast) {
    if (!ast) {
        return "// Error: Invalid program structure\n";
    }
    
    std::stringstream ss;
    Ident prog_name = ast->u.karelGrammer_.ident_1;
    
    ss << "// Karel program: " << prog_name << "\n\n";
    ss << "int main(void) {\n";
    increaseIndent();
    
    // TODO: Generate variable declarations from DeclBlock list
    
    // TODO: Generate statements from Stm list
    ListStm stmts = ast->u.karelGrammer_.liststm_;
    while (stmts) {
        if (stmts->stm_) {
            ss << indent() << genStatement(stmts->stm_) << ";\n";
        }
        stmts = stmts->liststm_;
    }
    
    ss << indent() << "return 0;\n";
    decreaseIndent();
    ss << "}\n";
    
    return ss.str();
}

std::string CodeGenerator::genDeclBlock(DeclBlock decl) {
    // TODO: Implement declaration block code generation
    return "";
}

std::string CodeGenerator::genVarDecl(VarDecl var) {
    // TODO: Implement variable declaration code generation
    return "";
}

std::string CodeGenerator::genRoutineDefinition(RoutineDefinitionBlock routine) {
    // TODO: Implement routine definition code generation
    return "";
}

std::string CodeGenerator::genStatement(Stm stmt) {
    if (!stmt) return "";
    
    switch (stmt->kind) {
        case is_SAssign: {
            // x = expression
            Ident var = stmt->u.sAssign_.ident_;
            Expression expr = stmt->u.sAssign_.expression_;
            return std::string(var) + " = " + genExpression(expr);
        }
        
        case is_SIfThen: {
            // IF condition THEN statements ENDIF
            Expression cond = stmt->u.sIfThen_.expression_;
            ListStm stmts = stmt->u.sIfThen_.liststm_;
            
            std::stringstream ss;
            ss << "if (" << genExpression(cond) << ") {\n";
            increaseIndent();
            while (stmts) {
                if (stmts->stm_) {
                    ss << indent() << genStatement(stmts->stm_) << ";\n";
                }
                stmts = stmts->liststm_;
            }
            decreaseIndent();
            ss << indent() << "}";
            return ss.str();
        }
        
        case is_SIfThenElse: {
            // IF condition THEN statements ELSE statements ENDIF
            Expression cond = stmt->u.sIfThenElse_.expression_;
            ListStm then_stmts = stmt->u.sIfThenElse_.liststm_1;
            ListStm else_stmts = stmt->u.sIfThenElse_.liststm_2;
            
            std::stringstream ss;
            ss << "if (" << genExpression(cond) << ") {\n";
            increaseIndent();
            while (then_stmts) {
                if (then_stmts->stm_) {
                    ss << indent() << genStatement(then_stmts->stm_) << ";\n";
                }
                then_stmts = then_stmts->liststm_;
            }
            decreaseIndent();
            ss << indent() << "} else {\n";
            increaseIndent();
            while (else_stmts) {
                if (else_stmts->stm_) {
                    ss << indent() << genStatement(else_stmts->stm_) << ";\n";
                }
                else_stmts = else_stmts->liststm_;
            }
            decreaseIndent();
            ss << indent() << "}";
            return ss.str();
        }
        
        case is_SWhile: {
            // WHILE condition DO statements ENDWHILE
            Expression cond = stmt->u.sWhile_.expression_;
            ListStm stmts = stmt->u.sWhile_.liststm_;
            
            std::stringstream ss;
            ss << "while (" << genExpression(cond) << ") {\n";
            increaseIndent();
            while (stmts) {
                if (stmts->stm_) {
                    ss << indent() << genStatement(stmts->stm_) << ";\n";
                }
                stmts = stmts->liststm_;
            }
            decreaseIndent();
            ss << indent() << "}";
            return ss.str();
        }
        
        case is_SForTo: {
            // FOR i = start TO end DO statements ENDFOR
            Ident var = stmt->u.sForTo_.ident_;
            Integer start = stmt->u.sForTo_.integer_1;
            Integer end = stmt->u.sForTo_.integer_2;
            ListStm stmts = stmt->u.sForTo_.liststm_;
            
            std::stringstream ss;
            ss << "for (" << var << " = " << start << "; " 
               << var << " <= " << end << "; " << var << "++) {\n";
            increaseIndent();
            while (stmts) {
                if (stmts->stm_) {
                    ss << indent() << genStatement(stmts->stm_) << ";\n";
                }
                stmts = stmts->liststm_;
            }
            decreaseIndent();
            ss << indent() << "}";
            return ss.str();
        }
        
        case is_SWrite: {
            // WRITE(items)
            ListWriteItem items = stmt->u.sWrite_.listwriteitem_;
            std::stringstream ss;
            ss << "karel_write(";
            bool first = true;
            while (items) {
                if (items->writeitem_) {
                    if (!first) ss << ", ";
                    // Simplified - WriteItem hat verschiedene Formen
                    ss << "/* write item */";
                    first = false;
                }
                items = items->listwriteitem_;
            }
            ss << ")";
            return ss.str();
        }
        
        case is_SReturn:
            return "return";
            
        case is_SReturnExp: {
            Expression expr = stmt->u.sReturnExp_.expression_;
            return "return " + genExpression(expr);
        }
        
        default:
            return "/* unimplemented statement type */";
    }
}

std::string CodeGenerator::genExpression(Expression expr) {
    if (!expr) return "0";
    
    switch (expr->kind) {
        case is_EInt:
            return std::to_string(expr->u.eInt_.integer_);
            
        case is_EDouble:
            return std::to_string(expr->u.eDouble_.double_);
            
        case is_EIdent:
            return std::string(expr->u.eIdent_.ident_);
            
        case is_EQString:
            return std::string("\"") + expr->u.eQString_.quotedstring_ + "\"";
            
        case is_EAdd:
            return genExpression(expr->u.eAdd_.expression_1) + " + " + 
                   genExpression(expr->u.eAdd_.expression_2);
                   
        case is_ESub:
            return genExpression(expr->u.eSub_.expression_1) + " - " + 
                   genExpression(expr->u.eSub_.expression_2);
                   
        case is_EMul:
            return genExpression(expr->u.eMul_.expression_1) + " * " + 
                   genExpression(expr->u.eMul_.expression_2);
                   
        case is_EDiv:
            return genExpression(expr->u.eDiv_.expression_1) + " / " + 
                   genExpression(expr->u.eDiv_.expression_2);
                   
        case is_EMOD:
            return genExpression(expr->u.eMOD_.expression_1) + " % " + 
                   genExpression(expr->u.eMOD_.expression_2);
                   
        case is_EEqual:
            return genExpression(expr->u.eEqual_.expression_1) + " == " + 
                   genExpression(expr->u.eEqual_.expression_2);
                   
        case is_ENEqual:
            return genExpression(expr->u.eNEqual_.expression_1) + " != " + 
                   genExpression(expr->u.eNEqual_.expression_2);
                   
        case is_ELess:
            return genExpression(expr->u.eLess_.expression_1) + " < " + 
                   genExpression(expr->u.eLess_.expression_2);
                   
        case is_ELeq:
            return genExpression(expr->u.eLeq_.expression_1) + " <= " + 
                   genExpression(expr->u.eLeq_.expression_2);
                   
        case is_Egret:
            return genExpression(expr->u.egret_.expression_1) + " > " + 
                   genExpression(expr->u.egret_.expression_2);
                   
        case is_Egeq:
            return genExpression(expr->u.egeq_.expression_1) + " >= " + 
                   genExpression(expr->u.egeq_.expression_2);
                   
        case is_EAnd:
            return genExpression(expr->u.eAnd_.expression_1) + " && " + 
                   genExpression(expr->u.eAnd_.expression_2);
                   
        case is_EOR:
            return genExpression(expr->u.eOR_.expression_1) + " || " + 
                   genExpression(expr->u.eOR_.expression_2);
                   
        case is_ENot:
            return "!" + genExpression(expr->u.eNot_.expression_);
            
        case is_EPlus:
            return "+" + genExpression(expr->u.ePlus_.expression_);
            
        case is_EMinus:
            return "-" + genExpression(expr->u.eMinus_.expression_);
            
        case is_EBrack:
            return "(" + genExpression(expr->u.eBrack_.expression_) + ")";
            
        // Math functions
        case is_EABS:
            return "fabs(" + genExpression(expr->u.eABS_.expression_) + ")";
            
        case is_ESin:
            return "sin(" + genExpression(expr->u.eSin_.expression_) + ")";
            
        case is_ECOS:
            return "cos(" + genExpression(expr->u.eCOS_.expression_) + ")";
            
        case is_ETan:
            return "tan(" + genExpression(expr->u.eTan_.expression_) + ")";
            
        case is_ESqrt:
            return "sqrt(" + genExpression(expr->u.eSqrt_.expression_) + ")";
            
        case is_ELn:
            return "log(" + genExpression(expr->u.eLn_.expression_) + ")";
            
        case is_EExp:
            return "exp(" + genExpression(expr->u.eExp_.expression_) + ")";
            
        case is_ERound:
            return "round(" + genExpression(expr->u.eRound_.expression_) + ")";
            
        case is_ETrunc:
            return "trunc(" + genExpression(expr->u.eTrunc_.expression_) + ")";
            
        default:
            return "/* unimplemented expression */";
    }
}

std::string CodeGenerator::mapType(DataTypes type) {
    if (!type) return "int";
    
    // TODO: Implement type mapping based on type->kind
    return "int";
}

std::string CodeGenerator::mapType(ParameterDataTypes type) {
    if (!type) return "int";
    
    // TODO: Implement parameter type mapping based on type->kind
    return "int";
}
