//------------------------------------------------------------------------------
// Tooling sample. Demonstrates:
//
// * How to write a simple source tool using libTooling.
// * How to use RecursiveASTVisitor to find interesting AST nodes.
// * How to use the Rewriter API to rewrite the source code.
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//------------------------------------------------------------------------------
#include <sstream>
#include <string>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h" //added
#include "clang/Driver/Options.h" //added
#include "clang/Lex/Lexer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
using namespace std;
using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
vector<FunctionDecl> functiondeclvector;
vector<FunctionDecl> callRvector;
vector<FunctionDecl> callEvector;
string CallerFunc = ""; 
static llvm::cl::OptionCategory ToolingSampleCategory("Tooling Sample");


//get the source code of the specific parts of AST
template <typename T>
static std::string getText(const SourceManager &SourceManager, const T &Node) {
  SourceLocation StartSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocStart());
  SourceLocation EndSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocEnd());
  if (!StartSpellingLocation.isValid() || !EndSpellingLocation.isValid()) {
    return std::string();
  }
  bool Invalid = true;
  const char *Text =
      SourceManager.getCharacterData(StartSpellingLocation, &Invalid);
  if (Invalid) {
    return std::string();
  }
  std::pair<FileID, unsigned> Start =
      SourceManager.getDecomposedLoc(StartSpellingLocation);
  std::pair<FileID, unsigned> End =
      SourceManager.getDecomposedLoc(Lexer::getLocForEndOfToken(
          EndSpellingLocation, 0, SourceManager, LangOptions()));
  if (Start.first != End.first) {
    // Start and end are in different files.
    return std::string();
  }
  if (End.second < Start.second) {
    // Shuffling text with macros may cause this.
    return std::string();
  }
  return std::string(Text, End.second - Start.second);
}

class FindNamedCallVisitor : public RecursiveASTVisitor<FindNamedCallVisitor> {
 public:
  explicit FindNamedCallVisitor(Rewriter &R) : TheRewriter(R){} 
  virtual bool VisitFunctionDecl(FunctionDecl *func)
  {
    CallerFunc = func->getNameInfo().getName().getAsString();
    return true;
  }
  virtual bool VisitCallExpr(CallExpr *CallExpression) {
    if(CallExpression != NULL){
      QualType q = CallExpression->getType();
      const Type *t = q.getTypePtrOrNull();
      if (t != NULL) {
	FunctionDecl *func = CallExpression->getDirectCallee();
	const std::string funcName = func->getNameInfo().getAsString();
	cout<<funcName<< " called by " <<CallerFunc;
      }
    }

    return true;
  }

 private:
  Rewriter &TheRewriter;
};

class FindNamedCallConsumer : public clang::ASTConsumer {
 public:
  FindNamedCallConsumer(Rewriter &R) : Visitor(R){}
  
  bool HandleTopLevelDecl(DeclGroupRef DR) override {
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      // Traverse the declaration using our AST visitor.
      Visitor.TraverseDecl(*b);
      //(*b)->dump();
    }
    return true;
  }

private:
  FindNamedCallVisitor Visitor;
  
};

class FindNamedCallAction : public clang::ASTFrontendAction {
 public:
  FindNamedCallAction() {}
    void EndSourceFileAction() override {
    SourceManager &SM = TheRewriter.getSourceMgr();
    llvm::errs() << "** EndSourceFileAction for: "
                 << SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";


    //handle each function definition
    for(unsigned funcid = 0; funcid < functiondeclvector.size(); funcid++){
      FunctionDecl *f = &functiondeclvector[funcid];
      Stmt *FuncBody = f->getBody();

      // Function name
      DeclarationName DeclName = f->getNameInfo().getName();
      std::string FuncName = DeclName.getAsString();

      FullSourceLoc funcstart = Context->getFullLoc(FuncBody->getLocStart());
      FullSourceLoc funcend = Context->getFullLoc(FuncBody->getLocEnd());
      unsigned startln = funcstart.getSpellingLineNumber();
      unsigned endln = funcend.getSpellingLineNumber();
      ofstream myfile;
      myfile.open (FuncName + ".dot");
      cout<< (FuncName + ".dot")<<endl;
      myfile << "digraph unnamed {\n";
      
      
      myfile << "Node1 [shape=record,label=\"{ [(ENTRY)]\\l}\"];\n";
      

      string nodebeforeif = "";
      // Here we only handle variable definition. 
      // To make it fully functional, other statment has to be handled too.

      /*if(isa<DeclStmt>(stmtvector[stmtid])) {       
          DeclStmt *ds = cast<DeclStmt>(stmtvector[stmtid]);
          VarDecl *vd = cast<VarDecl>(ds->getSingleDecl());*/
      //if(vd){
      //nodebeforeif = nodebeforeif + getText(TheRewriter.getSourceMgr(), *vd) + "\\l";
      //}
	  /*} 
	    }*/
      myfile << "Node2 [shape=record,label=\"" + nodebeforeif + "\"]\n";
      myfile << "Node1 -> Node2;\n";
      //We assume one if statement here
      FunctionDecl *func = 0;
      string youwin = func->getNameInfo().getName().getAsString();
      //IfStmt *IfStatement = ifstmtvector[0];
      string condition = getText(TheRewriter.getSourceMgr(), *func);
      myfile << "Node3 [shape=record,label=\"" + youwin  + "\"]\n";
      myfile << "Node2 -> Node3;\n";
      //Stmt * = IfStatement->getThen();
      //string thenpart = getText(SM, *Then);
      CallExpr *CallExpression = 0;
      CallExpression->getType();
      FunctionDecl *fun = 0;
      fun = CallExpression->getDirectCallee();
      string itsfun = fun ->getNameInfo().getAsString();
      myfile << "Node4 [shape=record,label=\"" + itsfun + "\"]\n";
      myfile << "Node3 -> Node4;\n";
      myfile << "Node6 [shape=record,label=\"{ [(Exit)]\\l}\"];\n";
      myfile << "Node4 -> Node6;\n";
      /*Stmt *Else = IfStatement->getElse();
      if (Else){
      string elsepart = getText(SM, *Else);
        myfile << "Node5 [shape=record,label=\"" + elsepart + "\"]\n";
        myfile << "Node3 -> Node5;\n";
        myfile << "Node5 -> Node6;\n";
	}*/
      
      myfile << "}\n";
      myfile.close();

    }
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    llvm::errs() << "** Creating AST consumer for: " << file << "\n";
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    Context = &CI.getASTContext();
    return llvm::make_unique< FindNamedCallConsumer>(TheRewriter);
  }

private:
  Rewriter TheRewriter;
  ASTContext *Context;
};

static llvm::cl::OptionCategory MyToolCategory("my-tool options");

int main(int argc, const char **argv) {
  const std::string fName = "doSomething";

  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  // run the Clang Tool, creating a new FrontendAction (explained below)
  int result = Tool.run(newFrontendActionFactory<FindNamedCallAction>().get());
  return result;
}
