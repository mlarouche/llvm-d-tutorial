/// LLVM Kaleidoscope Tutorial in D
/// Converted by Michaël Larouche <michael.larouche@gmail.com> 
module test.llvmKaleidoscope;

import llvm.c.Analysis;
import llvm.c.Core;
import llvm.c.ExecutionEngine;
import llvm.c.Initialization;
import llvm.c.transforms.Scalar;
import llvm.c.Target;
import std.ascii;
import std.conv;
import std.stdio;
import std.string;
import std.typecons;
import std.c.stdio;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token
{
	tok_eof = -1,
	
	tok_def = -2, tok_extern = -3,
	
	tok_identifier = -4, tok_number = -5,
	
	tok_if = -6, tok_then = -7, tok_else = -8,
	tok_for = -9, tok_in = -10,
	
	tok_binary = -11, tok_unary = -12,
	tok_var = -13
}

string IdentifierStr; // Filled in if tok_identifier
double NumVal; // Filled in if tok_number

int readChar()
{
	return getchar();
}

/// gettok - Return the next token from standard input.
int gettok()
{
	static int LastChar = ' ';
	
	// Skip any whitespace
	while(isWhite(LastChar))
	{
		LastChar = readChar();
	}
	
	// identifier: [a-zA-Z][a-zA-Z0-9]*
	if(isAlpha(LastChar))
	{
		IdentifierStr = "";
		IdentifierStr ~= LastChar;
		LastChar = readChar();
		while(isAlphaNum(LastChar))
		{
			IdentifierStr ~= LastChar;
			LastChar = readChar();
		}
		
		switch(IdentifierStr)
		{
			case "def":
				return Token.tok_def;
			case "extern":
				return Token.tok_extern;
			case "if":
				return Token.tok_if;
			case "then":
				return Token.tok_then;
			case "else":
				return Token.tok_else;
			case "for":
				return Token.tok_for;
			case "in":
				return Token.tok_in;
			case "binary":
				return Token.tok_binary;
			case "unary":
				return Token.tok_unary;
			case "var":
				return Token.tok_var;
			default:
				return Token.tok_identifier;
		}
	}
	
	// Number: [0-9.]+
	if(isDigit(LastChar) || LastChar == '.')
	{
		string NumStr;
		do
		{
			NumStr ~= LastChar;
			LastChar = readChar();
		}
		while(isDigit(LastChar) || LastChar == '.');
		
		NumVal = to!double(NumStr);
		return Token.tok_number;
	}
	
	// Comment until end of line.
	if(LastChar == '#')
	{
		do
		{
			LastChar = readChar();
		}
		while(LastChar != EOF && LastChar != '\n' && LastChar != '\r');
		
		if(LastChar != EOF)
		{
			return gettok();
		}
	}
	
	// Check for end of file.  Don't eat the EOF.
	if(LastChar == EOF)
	{
		return Token.tok_eof;
	}
	
	// Otherwise, just return the character as its ascii value.
	int ThisChar = LastChar;
	LastChar = readChar();
	return cast(int)ThisChar;
}


LLVMValueRef CreateEntryBlockAlloca(LLVMValueRef theFunction, string varName)
{
	auto backupCurrentBlock = LLVMGetInsertBlock(Builder);
	
	LLVMPositionBuilderAtEnd(Builder, LLVMGetFirstBasicBlock(theFunction));
	
	auto allocaValue = LLVMBuildAlloca(Builder, LLVMDoubleType(), varName.toStringz());
	
	LLVMPositionBuilderAtEnd(Builder, backupCurrentBlock);
	
	return allocaValue;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
// Code generation
//===----------------------------------------------------------------------===//

LLVMModuleRef TheModule;
LLVMValueRef[string] NamedValues;
LLVMBuilderRef Builder;
LLVMPassManagerRef TheFPM;

/// ExprAST - Base class for all expression nodes.
class ExprAST
{
	LLVMValueRef Codegen()
	{
		return null;
	}
}

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : ExprAST
{
public:
	this(double val)
	{
		Val = val;
	}
	
	LLVMValueRef Codegen()
	{
		return LLVMConstReal(LLVMDoubleType(), Val);
	}
private:
	double Val;
}

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : ExprAST
{
public:
	this(string name)
	{
		Name = name;
	}
	
	LLVMValueRef Codegen()
	{
		// Loop this variable in the function
		if(Name in NamedValues)
		{
			auto v = NamedValues[Name];
			
			// Load the value.
			return LLVMBuildLoad(Builder, v, Name.toStringz());
		}
		else
		{
			return ErrorV("Unknown variable name");
		}
	}
	
	string getName()
	{
		return Name;
	}
	
private:
	string Name;
}

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : ExprAST
{
public:
	this(char op, ExprAST lhs, ExprAST rhs)
	{
		Op = op;
		LHS = lhs;
		RHS = rhs;
	}
	
	LLVMValueRef Codegen()
	{
		// Special case '=' because we don't want to emit the LHS as an expression.
		if(Op == '=')
		{
			// Asignment requires the LHS to be an identifier.
			if(cast(VariableExprAST)LHS)
			{
				VariableExprAST lhse = cast(VariableExprAST)LHS;

				// Codegen the RHS.
				auto val = RHS.Codegen();

				// Look up the variable.
				if(lhse.getName() !in NamedValues)
				{
					return ErrorV("Unknown variable in assignment.");
				}
				
				auto variable = NamedValues[lhse.getName()];
				LLVMBuildStore(Builder, val, variable);
				return val;
			}
			else
			{
				return ErrorV("destination of '=' must be a variable");
			}
		}
		
		auto l = LHS.Codegen();
		auto r = RHS.Codegen();
		if(l is null || r is null)
		{
			return null;
		}
		
		switch(Op)
		{
			case '+':
				return LLVMBuildFAdd(Builder, l, r, "addtmp");
			case '-':
				return LLVMBuildFSub(Builder, l, r, "subtmp");
			case '*':
				return LLVMBuildFMul(Builder, l, r, "multmp");
			case '/':
				return LLVMBuildFDiv(Builder, l, r, "divtmp");
			case '<':
				l = LLVMBuildFCmp(Builder, LLVMRealPredicate.ULT, l, r, "cmp");
				// Convert bool 0/1 to double 0.0 or 1.0
				return LLVMBuildUIToFP(Builder, l, LLVMDoubleType(), "booltmp");
		
			// If it wasn't a builtin binary operator, it must be a user defined one. Emit
			// a call to it.
			default:
				string operatorFunctionName = "binary" ~ Op;
				auto f = LLVMGetNamedFunction(TheModule, operatorFunctionName.toStringz());
				assert(f, "binary operator not found !");
				
				LLVMValueRef[2] ops = [l, r];
				return LLVMBuildCall(Builder, f, ops.ptr, cast(uint)ops.length, "binop");
		}
	}
private:
	char Op;
	ExprAST LHS;
	ExprAST RHS;
}

/// CallExprAST - Expression class for function calls.
class CallExprAST : ExprAST
{
public:
	this(string callee, ExprAST[] args)
	{
		Callee = callee;
		Args = args;
	}
	
	LLVMValueRef Codegen()
	{
		// Look up the name in the global module table.
		LLVMValueRef calleeF = LLVMGetNamedFunction(TheModule, Callee.toStringz());
		if(calleeF is null)
		{
			return ErrorV("Unknow function referenced");
		}
		
		// If argument mismatch error.
		if(LLVMCountParams(calleeF) != Args.length)
		{
			return ErrorV("Incorrect number of arguments passed");
		}
		
		LLVMValueRef[] argsV;
		foreach(arg; Args)
		{
			auto generatedValue = arg.Codegen();
			if(generatedValue is null)
			{
				return null;
			}
			
			argsV ~= generatedValue;
		}
		
		return LLVMBuildCall(Builder, calleeF, argsV.ptr, cast(uint)argsV.length, "calltmp");
	}
private:
	string Callee;
	ExprAST[] Args;
}

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeAST
{
public:
	this(string name, string[] args, bool _isOperator = false, uint _precedence = 0)
	{
		Name = name;
		Args = args;
		isOperator = _isOperator;
		Precedence = _precedence;
	}
	
	@property bool isUnaryOp()
	{
		return isOperator && Args.length == 1;
	}
	
	@property bool isBinaryOp()
	{
		return isOperator && Args.length == 2;
	}
	
	@property char operatorName()
	{
		assert(isUnaryOp() || isBinaryOp());
		return Name[$-1];
	}
	
	@property uint binaryPrecedence()
	{
		return Precedence;
	}
	
	/// CreateArgumentAllocas - Create an alloca for each argument and register the
	/// argument in the symbol table so that references to it will succeed.
	void CreateArgumentAllocas(LLVMValueRef f)
	{
		LLVMValueRef[] params;
		params.length = LLVMCountParams(f);
		LLVMGetParams(f, params.ptr);
		
		foreach(index, arg; params)
		{
			// Create an alloca for this variable.
			auto alloca = CreateEntryBlockAlloca(f, Args[index]);
			
			// Store the initial value into the alloca.
			LLVMBuildStore(Builder, arg, alloca);

			// Add arguments to variable symbol table.
			NamedValues[Args[index]] = alloca;
		}
	}
	
	LLVMValueRef Codegen()
	{
		 // Make the function type:  double(double,double) etc.
		LLVMTypeRef[] argTypes;
		foreach(arg; Args)
		{
			argTypes ~= LLVMDoubleType();
		}
		
		auto ft = LLVMFunctionType(LLVMDoubleType(), argTypes.ptr, cast(uint)argTypes.length, false);
		auto f = LLVMAddFunction(TheModule, Name.toStringz(), ft);
		
		// If F conflicted, there was already something named 'Name'.  If it has a
		// body, don't allow redefinition or reextern.
		if(to!string(LLVMGetValueName(f)) != Name)
		{
			// Delete the one we just made and get the existing one.
			LLVMDeleteFunction(f);
			f = LLVMGetNamedFunction(TheModule, Name.toStringz());
			
			// If F already has a body, reject this.
			if(LLVMCountBasicBlocks(f) > 0)
			{
				return ErrorV("Redefinition of function");
			}
			
			// If F took a different number of args, reject.
			if(LLVMCountParams(f) != Args.length)
			{
				return ErrorV("Redefinition of function with differents number of arguments");
			}
		}
		
		// Set names for all arguments
		LLVMValueRef[] params;
		params.length = LLVMCountParams(f);
		LLVMGetParams(f, params.ptr);
		
		foreach(index, arg; params)
		{
			LLVMSetValueName(arg, Args[index].toStringz());
		}
		
		return f;
	}
private:
	string Name;
	string[] Args;
	bool isOperator;
	uint Precedence;
}

/// FunctionAST - This class represents a function definition itself.
class FunctionAST
{
public:
	this(PrototypeAST proto, ExprAST _body)
	{
		Proto = proto;
		Body = _body;
	}
	
	LLVMValueRef Codegen()
	{
		NamedValues.clear();
		
		auto theFunction = Proto.Codegen();
		if(theFunction is null)
		{
			return null;
		}
		
		// If this is an operator, install it.
		if(Proto.isBinaryOp)
		{
			BinopPrecedence[cast(int)Proto.operatorName] = Proto.binaryPrecedence;
		}
		
		// Create a new basic block to start insertion into.
		auto basicBlock = LLVMAppendBasicBlock(theFunction, "entry");
		LLVMPositionBuilderAtEnd(Builder, basicBlock);
		
		// Add all arguments to the symbol table and create their allocas.
		Proto.CreateArgumentAllocas(theFunction);
		
		auto returnValue = Body.Codegen();
		if(returnValue)
		{
			//LLVMVerifyFunction(returnValue, LLVMVerifierFailureAction.PrintMessage);
			
			// Finish off the function.
			LLVMBuildRet(Builder, returnValue);
			
			// Valide the generated code, checking for consistency.
			LLVMVerifyFunction(theFunction, LLVMVerifierFailureAction.PrintMessage);
			
			// Optimize the function.
			LLVMRunFunctionPassManager(TheFPM, theFunction);
			
			return theFunction;
		}
		
		// Error reading body, remove function.
		LLVMDeleteFunction(theFunction);
		
		if (Proto.isBinaryOp)
		{
		    BinopPrecedence.remove(cast(int)Proto.operatorName);
		}
		
		return null;
	}
	
private:
	PrototypeAST Proto;
	ExprAST Body;
}

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : ExprAST
{
public:
	this(ExprAST cond, ExprAST then, ExprAST _else)
	{
		Cond = cond;
		Then = then;
		Else = _else;
	}
	
	
	LLVMValueRef Codegen()
	{
		auto condV = Cond.Codegen();
		if(condV is null)
		{
			return null;
		}
		
		// Convert condition to a bool by comparing equal to 0.0.
		condV = LLVMBuildFCmp(Builder, LLVMRealPredicate.ONE, condV, LLVMConstReal(LLVMDoubleType(), 0.0), "ifcond");
		
		auto theFunction = LLVMGetBasicBlockParent(LLVMGetInsertBlock(Builder));
		
		// Create blocks for the then and else cases.  Insert the 'then' block at the
		// end of the function.
		auto thenBB = LLVMAppendBasicBlock(theFunction, "then");
		auto elseBB = LLVMAppendBasicBlock(theFunction, "else");
		auto mergeBB = LLVMAppendBasicBlock(theFunction, "ifcont");
		
		LLVMBuildCondBr(Builder, condV, thenBB, elseBB);
		
		// Emit then value
		LLVMPositionBuilderAtEnd(Builder, thenBB);
		
		auto thenV = Then.Codegen();
		if(thenV is null)
		{
			return null;
		}
		
		LLVMBuildBr(Builder, mergeBB);
		// Codegen of 'Then' can change the current block, update thenBB for the PHI.
		thenBB = LLVMGetInsertBlock(Builder);
		
		// Emit else block
		auto lastFunctionBlock = LLVMGetLastBasicBlock(theFunction);
		LLVMMoveBasicBlockAfter(elseBB, lastFunctionBlock);
		LLVMPositionBuilderAtEnd(Builder, elseBB);
		
		auto elseV = Else.Codegen();
		if(elseV is null)
		{
			return null;
		}
		
		LLVMBuildBr(Builder, mergeBB);
		
		// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
		elseBB = LLVMGetInsertBlock(Builder);
		
		// Emit merge block
		lastFunctionBlock = LLVMGetLastBasicBlock(theFunction);
		LLVMMoveBasicBlockAfter(mergeBB, lastFunctionBlock);
		LLVMPositionBuilderAtEnd(Builder, mergeBB);
		
		auto phiNode = LLVMBuildPhi(Builder, LLVMDoubleType(), "iftmp");
		
		LLVMValueRef[2] incomingValues;
		incomingValues[0] = thenV;
		incomingValues[1] = elseV;
		LLVMBasicBlockRef[2] incomingBlocks;
		incomingBlocks[0] = thenBB;
		incomingBlocks[1] = elseBB;
		LLVMAddIncoming(phiNode, incomingValues.ptr, incomingBlocks.ptr, incomingValues.length);
		
		return phiNode;
	}
private:
	ExprAST Cond, Then, Else;
}

/// ForExprAST - Expression class for for/in.
class ForExprAST : ExprAST
{
public:
	this(string varName, ExprAST start, ExprAST end, ExprAST step, ExprAST _body)
	{
		VarName = varName;
		Start = start;
		End = end;
		Step = step;
		Body = _body;
	}
	
	LLVMValueRef Codegen()
	{
		// Output this as:
		//   var = alloca double
		//   ...
		//   start = startexpr
		//   store start -> var
		//   goto loop
		// loop: 
		//   ...
		//   bodyexpr
		//   ...
		// loopend:
		//   step = stepexpr
		//   endcond = endexpr
		//
		//   curvar = load var
		//   nextvar = curvar + step
		//   store nextvar -> var
		//   br endcond, loop, endloop
		// outloop:
		
		// Make the new basic block for the loop header, inserting after current block.
		auto theFunction = LLVMGetBasicBlockParent(LLVMGetInsertBlock(Builder));
		
		// Create an alloca for the variable in the entry block
		auto alloca = CreateEntryBlockAlloca(theFunction, VarName);
		
		// Emit the start code first, without 'variable in scope
		auto startVal = Start.Codegen();
		if(startVal is null)
		{
			return null;
		}
		
		// Store the value into the alloca
		LLVMBuildStore(Builder, startVal, alloca);
		
		// Make the new basic block for the loop header, inserting after current
	 	// block.
		auto loopBB = LLVMAppendBasicBlock(theFunction, "loop");
		
		// Insert an explicit fall through from the current block to the loopBB
		LLVMBuildBr(Builder, loopBB);
		
		// Start insertion in loopBB
		LLVMPositionBuilderAtEnd(Builder, loopBB);
		
		// Within the loop, the variable is definied equal to the PHI node.
		// If it shadows an existing variable, we have to restore it, so save it now.
		LLVMValueRef oldVal = null;
		if(VarName in NamedValues)
		{
			oldVal = NamedValues[VarName];
		}
		NamedValues[VarName] = alloca;
		
		// Emit the body of the loop. This, like any other expr, can change the current BB.
		// Note that we ignore the value computed by the body, but don't allow an error.
		if(Body.Codegen() is null)
		{
			return null;
		}
		
		// Emit the step value.
		LLVMValueRef stepVal;
		if(Step)
		{
			stepVal = Step.Codegen();
			if(stepVal)
			{
				return null;
			}
		}
		else
		{
			stepVal = LLVMConstReal(LLVMDoubleType(), 1.0);
		}
		
		// Compute the end condition.
		auto endCond = End.Codegen();
		if(endCond is null) 
		{
			return null;
		}
		
		// Reload, increment, and restore the alloca.
		// This handles the case where the body of the loop mutates the variable.
		auto curVar = LLVMBuildLoad(Builder, alloca, "curvar");
		auto nextVar = LLVMBuildFAdd(Builder, curVar, stepVal, "nextvar");
		LLVMBuildStore(Builder, nextVar, alloca);
		
		// Convert condition to a bool by comparing equal to 0.0.
		endCond = LLVMBuildFCmp(Builder, LLVMRealPredicate.ONE, endCond, LLVMConstReal(LLVMDoubleType(), 0.0), "loopcond");
		
		// Create the "after loop" block and insert it.
		auto afterBB = LLVMAppendBasicBlock(theFunction, "afterloop");
		
		// Insert the conditional branch into the end of loopEndBB
		LLVMBuildCondBr(Builder, endCond, loopBB, afterBB);
		
		// Any new code will be inserted in afterBB
		LLVMPositionBuilderAtEnd(Builder, afterBB);
		
		// Restore the unshadowed variable.
		if(oldVal)
		{
			NamedValues[VarName] = oldVal;
		}
		else
		{
			NamedValues.remove(VarName);
		}
		
		// For expr always returns 0.0
		return LLVMConstReal(LLVMDoubleType(), 0.0);
	}
	
private:
	string VarName;
	ExprAST Start, End, Step, Body;
}

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : ExprAST
{
public:
	this(char opcode, ExprAST operand)
	{
		Opcode = opcode;
		Operand = operand;
	}
	
	LLVMValueRef Codegen()
	{
		auto operandV = Operand.Codegen();
		string functionName = "unary" ~ Opcode;
		auto f = LLVMGetNamedFunction(TheModule, functionName.toStringz());
		if(f)
		{
			LLVMValueRef[] args;
			return LLVMBuildCall(Builder, f, args.ptr, 0, "unop");
		}
		
		return ErrorV("Unknown unary operator");
	}
private:
	char Opcode;
	ExprAST Operand;
}

alias Tuple!(string, ExprAST) VarExprTuple;

/// VarExprAST - Expression class for var/in
class VarExprAST : ExprAST
{
public:
	this(VarExprTuple[] varNames, ExprAST _body)
	{
		VarNames = varNames;
		Body = _body;
	}
	
	LLVMValueRef Codegen()
	{
		LLVMValueRef[string] oldBindings;
		
		auto theFunction = LLVMGetBasicBlockParent(LLVMGetInsertBlock(Builder));
		
		// Register all variable and emit their initializer
		foreach(varExpr; VarNames)
		{
			string varName = varExpr[0];
			ExprAST init = varExpr[1];
			
			// Emit the initializer before adding the variable to scope, this prevents
			// the initializer from referencing the variable itself, and permits stuff
			// like this:
			// var a = 1 in
			//     var a = a in ... # refers to outer 'a'
			LLVMValueRef initVal;
			if(init)
			{
				initVal = init.Codegen();
				if(initVal is null)
				{
					return null;
				}
			}
			else
			{
				initVal = LLVMConstReal(LLVMDoubleType(), 0.0);
			}
			
			auto alloca = CreateEntryBlockAlloca(theFunction, varName);
			LLVMBuildStore(Builder, initVal, alloca);
			
			// Remember the old variable binding so that we can restore the binding when
			// we unrecurse.
			if(varName in NamedValues)
			{
				oldBindings[varName] = NamedValues[varName];
			}
			
			// Remember this binding
			NamedValues[varName] = alloca;
		}
		
		// Codegen the body, now that all vars in scope.
		auto bodyVal = Body.Codegen();
		if(bodyVal is null)
		{
			return null;
		}
		
		// Pop all our variables from scope
		foreach(varExpr; VarNames)
		{
			string varName = varExpr[0];
			if(varName in oldBindings)
			{
				NamedValues[varName] = oldBindings[varName];
			}
		}
		
		// Return the body computation.
		return bodyVal;
	}
private:
	VarExprTuple[] VarNames;
	ExprAST Body;
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
int CurTok;
int getNextToken()
{
	CurTok = gettok();
	return CurTok;
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
int[int] BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int GetTokPrecedence()
{
	if(!isASCII(CurTok))
	{
		return -1;
	}
	
	if(!(CurTok in BinopPrecedence))
	{
		return -1;
	}
	
	// Make sure it's a declared binop.
	int tokPrec = BinopPrecedence[CurTok];
	if (tokPrec <= 0)
	{
		return -1;
	}
	
	return tokPrec;
}

/// Error* - These are little helper functions for error handling.
ExprAST Error(string str)
{
	writeln("Error: ", str);
	return null;
}

PrototypeAST ErrorP(string str)
{
	Error(str);
	return null;
}

FunctionAST ErrorF(string str)
{
	Error(str);
	return null;
}

LLVMValueRef ErrorV(string str)
{
	Error(str);
	return null;
}

/// numberexpr ::= number
ExprAST ParseNumberExpr()
{
	ExprAST result = new NumberExprAST(NumVal);
	getNextToken(); // consume the number
	return result;
}

/// parenexpr ::= '(' expression ')'
ExprAST ParseParenExpr()
{
	getNextToken(); // Eat (
	ExprAST v = ParseExpression();
	if(v is null)
	{
		return null;
	}
	
	if(CurTok != ')')
	{
		return Error("expected )");
	}
	getNextToken(); // Eat )
	return v;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
ExprAST ParseIdentifierExpr()
{
	string IdName = IdentifierStr;
	
	getNextToken(); // Eat identifier
	
	if(CurTok != '(')
	{
		return new VariableExprAST(IdName);
	}
	
	// Call
	getNextToken(); // eat (
	
	ExprAST[] args;
	if(CurTok != ')')
	{
		while(1)
		{
			ExprAST arg = ParseExpression();
			if(arg is null)
			{
				return null;
			}
			args ~= arg;
			
			if(CurTok == ')')
				break;
				
			if(CurTok != ',')
			{
				return Error("Expected ) or , in argument list");
			}
			
			getNextToken();
		}
	}
	
	// Eat the )
	getNextToken();
	
	return new CallExprAST(IdName, args);
}

/// primary
/// 	::= identifierexpr
/// 	::= numberexpr
/// 	::= parenexpr
/// 	::= ifexpr
/// 	::= forexpr
/// 	::= varexpr
ExprAST ParsePrimary()
{
	switch(CurTok)
	{
		case Token.tok_identifier:
			return ParseIdentifierExpr();
		case Token.tok_number:
			return ParseNumberExpr();
		case '(':
			return ParseParenExpr();
		case Token.tok_if:
			return ParseIfExpr();
		case Token.tok_for:
			return ParseForExpr();
		case Token.tok_var:
			return ParseVarExpr();
		default:
			return Error("Unknown token when expecting an expression.");
	}
}

/// unary
/// 	::= primary
/// 	:: '!' unary
ExprAST ParseUnary()
{
	// If the current token is not an operator, it must be a primary expr.
	if(!isASCII(CurTok) || CurTok == '(' || CurTok == ',')
	{
		return ParsePrimary();
	}
	
	// If this is a unary operator, read it.
	int opc = CurTok;
	getNextToken();
	auto operand = ParseUnary();
	if(operand)
	{
		return new UnaryExprAST(cast(char)opc, operand);
	}
	
	return null;
}

/// binoprhs
/// 	::= ('+' unary)*
ExprAST ParseBinOpRHS(int ExprPrec, ExprAST lhs)
{
	// If this is a binop, find its precedence.
	while(1)
	{
		int tokPrec = GetTokPrecedence();
		
		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if(tokPrec < ExprPrec)
		{
			return lhs;
		}
		
		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop
		
		// Parse the unary expression after the binary operator.
		ExprAST rhs = ParseUnary();
		if(rhs is null)
		{
			return null;
		}
		
		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int nextPrec = GetTokPrecedence();
		if(tokPrec < nextPrec)
		{
			rhs = ParseBinOpRHS(tokPrec+1, rhs);
			if(rhs is null)
			{
				return null;
			}
		}
		
		// Merge LHS/RHS.
		lhs = new BinaryExprAST(cast(char)BinOp, lhs, rhs);
	}
}

/// expression
/// 	::= unary binoprhs
ExprAST ParseExpression()
{
	ExprAST lhs = ParseUnary();
	if(lhs is null)
	{
		return null;
	}
	
	return ParseBinOpRHS(0, lhs);
}

/// prototype
/// 	::= id '(' id* ')'
/// 	::= binary LETTER number? (id, id)
PrototypeAST ParsePrototype()
{
	string fnName;
	
	enum Kind
	{
		Identifier,
		Unary,
		Binary
	}
	Kind kind;
	
	uint binaryPrecedence = 30;
	
	switch(CurTok)
	{
		case Token.tok_identifier:
			fnName = IdentifierStr;
			kind = Kind.Identifier;
			getNextToken();
			break;
		case Token.tok_unary:
			getNextToken();
			if(!isASCII(CurTok))
			{
				return ErrorP("Expected unary operator");
			}
			fnName = "unary";
			fnName ~= cast(char)CurTok;
			kind = Kind.Unary;
			getNextToken();
			break;
		case Token.tok_binary:
			getNextToken();
			if(!isASCII(CurTok))
			{
				return ErrorP("Expected binary operator");
			}
			fnName = "binary";
			fnName ~= cast(char)CurTok;
			kind = Kind.Binary;
			getNextToken();
			
			// Read the precedence if present.
			if(CurTok == Token.tok_number)
			{
				if(NumVal < 1 || NumVal > 100)
				{
					return ErrorP("Invalid precedence: must be 1..100");
				}
				binaryPrecedence = cast(uint)NumVal;
				getNextToken();
			}
			break;
		default:
			return ErrorP("Expected function name in prototype");
	}
	
	if(CurTok != '(')
	{
		return ErrorP("Expected ( in prototype");
	}
	
	string[] argNames;
	while(getNextToken() == Token.tok_identifier)
	{
		argNames ~= IdentifierStr;
	}
	if(CurTok != ')')
	{
		return ErrorP("Expected ) in prototype");
	}
	
	// success.
	getNextToken(); // Eat ')'
	
	// Verify right number of names for operator.
	if(kind != Kind.Identifier && argNames.length != cast(uint)kind)
	{
		return ErrorP("Invalid number of operands for operator");
	}
	
	return new PrototypeAST(fnName, argNames, kind != Kind.Identifier, binaryPrecedence);
}

/// definition ::= 'def' prototype expression
FunctionAST ParseDefinition()
{
	getNextToken(); // eat def
	PrototypeAST proto = ParsePrototype();
	if(proto is null)
	{
		return null;
	}
	
	ExprAST e = ParseExpression();
	if(e)
	{
		return new FunctionAST(proto, e);
	}
	
	return null;
}

/// toplevelexpr ::= expression
FunctionAST ParseTopLevelExpr()
{
	ExprAST e = ParseExpression();
	if(e)
	{
		string[] empty;
		PrototypeAST proto = new PrototypeAST("", empty);
		return new FunctionAST(proto, e);
	}
	
	return null;
}

/// external ::= 'extern' prototype
PrototypeAST ParseExtern()
{
	getNextToken(); // eat extern
	return ParsePrototype();
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
ExprAST ParseIfExpr()
{
	getNextToken(); // Eat the if
	
	// Condition
	ExprAST cond = ParseExpression();
	if(cond is null)
	{
		return null;
	}
	
	if(CurTok != Token.tok_then)
	{
		return Error("Expected then");
	}
	getNextToken(); // Eat the then
	
	ExprAST then = ParseExpression();
	if(then is null)
	{
		return null;
	}
	
	if(CurTok != Token.tok_else)
	{
		return Error("Expected else");
	}
	
	getNextToken();
	
	ExprAST _else = ParseExpression();
	if(_else is null)
	{
		return null;
	}
	
	return new IfExprAST(cond, then, _else);
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
ExprAST ParseForExpr()
{
	getNextToken(); // Eat the for
	
	if(CurTok != Token.tok_identifier)
	{
		return Error("expected identifier after for");
	}
	
	string idName = IdentifierStr;
	getNextToken(); // eat identifier
	
	if(CurTok != '=')
	{
		return Error("Expected '=' after for");
	}
	getNextToken(); // eat '='
	
	ExprAST start = ParseExpression();
	if(start is null)
	{
		return null;
	}
	if(CurTok != ',')
	{
		return Error("Expected ',' after for start value");
	}
	getNextToken();
	
	ExprAST end = ParseExpression();
	if(end is null)
	{
		return null;
	}
	
	// The step value is optional
	ExprAST step = null;
	if(CurTok == ',')
	{
		getNextToken();
		step = ParseExpression();
		if(step is null)
		{
			return null;
		}
	}
	
	if(CurTok != Token.tok_in)
	{
		return Error("Expected 'in' after for");
	}
	getNextToken(); // Eat 'in'
	
	ExprAST _body = ParseExpression();
	if(_body is null)
	{
		return null;
	}
	
	return new ForExprAST(idName, start, end, step, _body);
}

/// varexpr ::= 'var' identifier ('=' expression)?
///                   (',' identifier ('=' expression)?)* 'in' expression
ExprAST ParseVarExpr()
{
	getNextToken(); // eat the var.
	VarExprTuple[] varNames;
	
	// At least one variable name is required.
	if(CurTok != Token.tok_identifier)
	{
		return Error("Expected identifier after var");
	}
	
	while(1)
	{
		string name = IdentifierStr;
		getNextToken(); // Eat identifier
		
		// Read the optional initializer.
		ExprAST init = null;
		if(CurTok == '=')
		{
			getNextToken(); // eat the '='
			
			init = ParseExpression();
			if(init is null)
			{
				return null;
			}
		}
		varNames ~= VarExprTuple(name, init);
		
		// End of var list, exit loop.
		if(CurTok != ',')
		{
			break;
		}
		
		getNextToken(); // Eat the ','.
		
		if(CurTok != Token.tok_identifier)
		{
			return Error("Expected identifier list after var.");
		}
	}
	
	// At this point, we have to have 'in'.
	if(CurTok != Token.tok_in)
	{
		return Error("Expected 'in' keyword after 'var'");
	}
	getNextToken(); // Eat 'in'
	
	ExprAST _body = ParseExpression();
	if(_body is null)
	{
		return null;
	}
	
	return new VarExprAST(varNames, _body);
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

LLVMExecutionEngineRef TheExecutionEngine;

void HandleDefinition()
{
	auto defAST = ParseDefinition();
	if(defAST)
	{
		auto defCodeGen = defAST.Codegen();
		if(defCodeGen)
		{
			writeln("Read function definition:");
			LLVMDumpValue(defCodeGen);
		}
	}
	else
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

void HandleExtern()
{
	auto externAST = ParseExtern();
	if(externAST)
	{
		auto externCodegen = externAST.Codegen();
		if(externCodegen)
		{
			writeln("Read extern:");
			LLVMDumpValue(externCodegen);
		}
	}
	else
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

void HandleTopLevelExpression()
{
	// Evaluate a top-level expression into an anonymous function.
	auto topLevelAST = ParseTopLevelExpr();
	
	if(topLevelAST)
	{
		auto topLevelCodegen = topLevelAST.Codegen();
		if(topLevelCodegen)
		{
			write("Read top-level expression:");
			LLVMDumpValue(topLevelCodegen);
			
			LLVMGenericValueRef[] args;
			
			// Execute function
			auto result = LLVMRunFunction(TheExecutionEngine, topLevelCodegen, 0, args.ptr);
			writeln("Evaluated to ", LLVMGenericValueToFloat(LLVMDoubleType(),result));
		}
	}
	else
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
void MainLoop()
{
	while(1)
	{
		write("ready> ");
		switch(CurTok)
		{
			case Token.tok_eof:
				return;
			// Ignore top-level semicolons.
			case ';':
				getNextToken();
				break;
			case Token.tok_def:
				HandleDefinition();
				break;
			case Token.tok_extern:
				HandleExtern();
				break;
			default:
				HandleTopLevelExpression();
				break;
		}
	}
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//
/// putchard - putchar that takes a double and returns 0.
extern (C) double putchard(double X)
{
	write(X);
	return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern (C) double printd(double X)
{
	writeln(X);
	return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
void main()
{
	// Link execution engines
	LLVMLinkInInterpreter();
	
	// Create module, builder and execution engine
	TheModule = LLVMModuleCreateWithName("Kaleidoscope JIT interpreter");
	Builder = LLVMCreateBuilder();
	
	char[1024] error;
	char* errorPtr = error.ptr;
	int result = LLVMCreateExecutionEngineForModule(&TheExecutionEngine, TheModule, &errorPtr);
	if(result == 1)
	{
		writeln(to!string(errorPtr));
		writeln("Cannot create execution engine ! Exiting...");
		return;
	}
	
	// Setup optimisation passes
	TheFPM = LLVMCreateFunctionPassManagerForModule(TheModule);
	LLVMAddTargetData(LLVMGetExecutionEngineTargetData(TheExecutionEngine), TheFPM);
	LLVMAddPromoteMemoryToRegisterPass(TheFPM);
	LLVMAddBasicAliasAnalysisPass(TheFPM);
	LLVMAddReassociatePass(TheFPM);
	LLVMAddInstructionCombiningPass(TheFPM);
	LLVMAddGVNPass(TheFPM);
	LLVMAddCFGSimplificationPass(TheFPM);
		
	// Install standard binary operators.
	// 1 is lowest precedence.
	BinopPrecedence['='] = 2;
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;
	BinopPrecedence['/'] = 40;
	
	// Read the first token
	write("ready> ");
	getNextToken();
	
	// Interpreter loop
	MainLoop();
	
	// Print out all of the generated code.
	LLVMDumpModule(TheModule);
} 
