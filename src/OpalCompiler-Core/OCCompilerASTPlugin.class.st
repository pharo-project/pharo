"
I am an abtract superclass for compiler plugings.

The compiler consists of multiple passes:

source 
	- [ Scanner/Parser ]  -> 
AST 
	- [ SemCheck ] -> 
AST   <<HERE>>
	- [ ASTTranslator ] -> 
IR 
	- [ IRBuilder ] ->
 CompiledMethod


These plugins are called <<HERE>>, that is, after semantic analysis before generating the IR.
They are sorted by #priority and handed a *copy* of the AST.



"
Class {
	#name : #OCCompilerASTPlugin,
	#superclass : #Object,
	#instVars : [
		'ast'
	],
	#category : #'OpalCompiler-Core-Translator'
}

{ #category : #private }
OCCompilerASTPlugin class >> priority [
	self subclassResponsibility
]

{ #category : #'instance creation' }
OCCompilerASTPlugin class >> transform: ast [
	^self new
		ast: ast;
		transform.
]

{ #category : #accessing }
OCCompilerASTPlugin >> ast: anAst [
	ast := anAst
]

{ #category : #api }
OCCompilerASTPlugin >> priority [
	^100 "default. Priority 0 is used by Reflectivity to be the last"
]

{ #category : #api }
OCCompilerASTPlugin >> transform [
	
	self subclassResponsibility.
]

{ #category : #api }
OCCompilerASTPlugin >> transform: anRBMethodNode [
	"by default do nothing"
	^ ast := anRBMethodNode.
]
