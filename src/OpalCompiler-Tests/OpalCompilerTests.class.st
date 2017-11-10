Class {
	#name : #OpalCompilerTests,
	#superclass : #TestCase,
	#category : #OpalCompiler-Tests-Misc
}

{ #category : #tests }
OpalCompilerTests >> testBindingsWithUppercaseNameDoNotOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: { #UndefinedObject -> Object } asDictionary;
		evaluate: 'UndefinedObject class'.
		
	self assert: result = UndefinedObject class.
]

{ #category : #tests }
OpalCompilerTests >> testEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: { #a -> 3 } asDictionary;
		evaluate: '1+a'.
		
	self assert: result = 4.
]

{ #category : #tests }
OpalCompilerTests >> testEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: { #MyVar -> 3 } asDictionary;
		evaluate: '1+MyVar'.
		
	self assert: result = 4.
]
