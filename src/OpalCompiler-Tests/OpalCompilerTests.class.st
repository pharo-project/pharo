Class {
	#name : #OpalCompilerTests,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
OpalCompilerTests >> testBindingsWithUppercaseNameDoNotOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: {(#UndefinedObject -> Object)} asDictionary;
		evaluate: 'UndefinedObject class'.
	self assert: result equals: UndefinedObject class
]

{ #category : #tests }
OpalCompilerTests >> testCompileEmbeddsSource [
	| result |
	result := Smalltalk compiler
		class: UndefinedObject;
		options: #( + #optionEmbeddSources );
		compile: 'tt ^3+4'.
	self assert: (result valueWithReceiver: nil arguments: #()) equals: 7.
	self deny: result trailer hasSourcePointer. "no sourcePointer"
	self assert: result trailer hasSource.		 "but source embedded"
	
	result := Smalltalk compiler
		class: UndefinedObject;
		options: #( - #optionEmbeddSources );
		compile: 'tt ^3+4'.
	self assert: (result valueWithReceiver: nil arguments: #()) equals: 7.
	self deny: result trailer hasSourcePointer. "no sourcePointer"
	self deny: result trailer hasSource.			 "and source embedded"
]

{ #category : #tests }
OpalCompilerTests >> testEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: {(#a -> 3)} asDictionary;
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : #tests }
OpalCompilerTests >> testEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: {(#MyVar -> 3)} asDictionary;
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]
