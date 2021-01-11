Class {
	#name : #OCDoitTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
OCDoitTest >> testDoItContextReadGlobal [
	"we can read a global from a doit when executing against a context"

	|  value |	
	value := OpalCompiler new
		source: 'Object';
		context: thisContext;
		evaluate.
		
	self assert: value equals: Object
]

{ #category : #tests }
OCDoitTest >> testDoItContextReadTemp [
	| tempForTesting value |
	"we can read this temp from a doit when executing against thisContext"
	tempForTesting := #someValue.
	
	value := OpalCompiler new
		source: 'tempForTesting';
		context: thisContext;
		evaluate.
		
	self assert: value equals: #someValue
]

{ #category : #tests }
OCDoitTest >> testDoitContextCheckClass [
	"if we create a parse tree for a doit in a context, the class should be correctly set"
	
	| ast |
	ast := OpalCompiler new
		source: 'tempForTesting';
		noPattern: true;
		context: thisContext;
		parseDoIt.
	"we expect that the class is set to the class of the context, which is self class"		
	self assert: ast methodClass equals: self class
]
