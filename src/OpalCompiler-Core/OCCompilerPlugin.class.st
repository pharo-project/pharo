"
I am an abtract superclass for compiler plugins.

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
They are sorted by #priority and handed the AST without making a copy (as plugins might just analyse the AST). 

IMPORTANT: If you modify the AST, you have to make a copy before!
"
Class {
	#name : #OCCompilerPlugin,
	#superclass : #Object,
	#category : #'OpalCompiler-Core-Plugins'
}

{ #category : #'accessing - defaults' }
OCCompilerPlugin class >> defaultPriority [
	"Use a high priority by default (a priority of 0 would be used by Reflectivity to be the last)"
	
	^ 100 
]

{ #category : #testing }
OCCompilerPlugin class >> isAbstract [ 

	 ^ self = OCCompilerPlugin
]

{ #category : #accessing }
OCCompilerPlugin >> priority [
	^self class defaultPriority
]

{ #category : #transforming }
OCCompilerPlugin >> transform: ast [
	
	self subclassResponsibility
]
