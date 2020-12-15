"
I am an abtract superclass for AST based compiler plugins providing a static transformation of an AST


"
Class {
	#name : #OCStaticCompilerPlugin,
	#superclass : #OCCompilerPlugin,
	#instVars : [
		'ast'
	],
	#category : #'OpalCompiler-Core-Plugins'
}

{ #category : #testing }
OCStaticCompilerPlugin class >> isAbstract [ 

	^ self = OCStaticCompilerPlugin 
]

{ #category : #'instance creation' }
OCStaticCompilerPlugin class >> transform: anAST [
	"Return a new instance of the receiver transforming the given AST"
	
	^self new transform: anAST
]

{ #category : #accessing }
OCStaticCompilerPlugin >> ast: anAST [
	ast := anAST
]

{ #category : #utilities }
OCStaticCompilerPlugin >> copyAST [
	"Utility method to make a copy of the AST before manipulating it"
	
	ast := ast copy
]

{ #category : #'private - transforming' }
OCStaticCompilerPlugin >> transform [
	"Subclasses override this method to actually provide the AST transformation. 
	 IMPORTANT: If you modify the AST, make sure to copy it before using #copyAST!"
	
	self subclassResponsibility
]

{ #category : #transforming }
OCStaticCompilerPlugin >> transform: anAST [
	ast := anAST.
	self transform.
	^ast
]
