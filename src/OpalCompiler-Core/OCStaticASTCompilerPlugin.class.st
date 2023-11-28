"
I am an abtract superclass for AST based compiler plugins providing a static transformation of an AST
"
Class {
	#name : 'OCStaticASTCompilerPlugin',
	#superclass : 'OCASTCompilerPlugin',
	#instVars : [
		'ast'
	],
	#category : 'OpalCompiler-Core-Plugins',
	#package : 'OpalCompiler-Core',
	#tag : 'Plugins'
}

{ #category : 'testing' }
OCStaticASTCompilerPlugin class >> isAbstract [

	^ self == OCStaticASTCompilerPlugin
]

{ #category : 'instance creation' }
OCStaticASTCompilerPlugin class >> transform: anAST [
	"Return a new instance of the receiver transforming the given AST"

	^self new transform: anAST
]

{ #category : 'accessing' }
OCStaticASTCompilerPlugin >> ast: anAST [

	ast := anAST
]

{ #category : 'utilities' }
OCStaticASTCompilerPlugin >> copyAST [
	"Utility method to make a copy of the AST before manipulating it"

	ast := ast copy
]

{ #category : 'private - transforming' }
OCStaticASTCompilerPlugin >> transform [
	"Subclasses override this method to actually provide the AST transformation.
	 IMPORTANT: If you modify the AST, make sure to copy it before using #copyAST!
	
	I act as a no-op to prevent loading errors. Image ClassA overrides #compiler to use PluginA from the same package. During the loading maybe the #transfrom method is not present yet and will break the loading if we use a subclass responsibility."

	
]

{ #category : 'transforming' }
OCStaticASTCompilerPlugin >> transform: anAST [

	ast := anAST.
	self transform.
	^ ast
]
