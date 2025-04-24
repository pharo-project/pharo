"
I am an abtract superclass for AST based compiler plugins providing a static transformation of an AST
"
Class {
	#name : #OCStaticASTCompilerPlugin,
	#superclass : #OCASTCompilerPlugin,
	#instVars : [
		#ast
	],
	#category : #'OpalCompiler-Core-Plugins'
}

{ #category : #testing }
OCStaticASTCompilerPlugin class >> isAbstract [

	^ self == OCStaticASTCompilerPlugin
]

{ #category : #private }
OCStaticASTCompilerPlugin class >> priority [
	self subclassResponsibility
]

{ #category : #'instance creation' }
OCStaticASTCompilerPlugin class >> transform: anAST [
	"Return a new instance of the receiver transforming the given AST"

	^self new transform: anAST
]

{ #category : #accessing }
OCStaticASTCompilerPlugin >> ast: anAST [

	ast := anAST
]

{ #category : #utilities }
OCStaticASTCompilerPlugin >> copyAST [
	"Utility method to make a copy of the AST before manipulating it"

	ast := ast copy
]

{ #category : #accessing }
OCStaticASTCompilerPlugin >> priority [
	^100 "default. Priority 0 is used by Reflectivity to be the last"
]

{ #category : #'private - transforming' }
OCStaticASTCompilerPlugin >> transform [
	"Subclasses override this method to actually provide the AST transformation.
	 IMPORTANT: If you modify the AST, make sure to copy it before using #copyAST!"

	self subclassResponsibility
]

{ #category : #transforming }
OCStaticASTCompilerPlugin >> transform: anAST [

	ast := anAST.
	self transform.
	^ ast
]
