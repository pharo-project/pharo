Class {
	#name : #OpalCompilerTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #'tests - bindings' }
OpalCompilerTest >> testArrayBindingsWithUppercaseNameDoOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: {(#UndefinedObject -> Object)};
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testArrayBindingsWriteGlobals [
	| result |
	result := Smalltalk compiler
		 bindings: {(#Object -> Point)};
       evaluate: 'Object := 42'.
	self assert: result equals: 42.
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: {(#a -> 3)};
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: {(#MyVar -> 3)};
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testBindingsWithUppercaseNameDoOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: {(#UndefinedObject -> Object)} asDictionary;
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testBindingsWriteGlobals [
	| result |
	result := Smalltalk compiler
		 bindings: {(#Object -> Point)} asDictionary;
       evaluate: 'Object := 42'.
	self assert: result equals: 42.
]

{ #category : #tests }
OpalCompilerTest >> testCompileEmbeddsSource [
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
OpalCompilerTest >> testCompileWithNilClass [
	"we shoud use UndefinedObject if the class is nil"
	| method |
	method := Smalltalk compiler compile: 'tst 1+2'.
	self assert: method methodClass equals: UndefinedObject.
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testCompilerUsingFullBlockClosureHasBlockAsLiteral [

  	| compiler method |
 	compiler := Smalltalk compiler.
 	compiler compilationContext encoderClass: EncoderForSistaV1.
 	compiler compilationContext compilerOptions: #(optionFullBlockClosure).

  	method := compiler compile: 'test #(1 2 3) do: [:e | e + 1]'.

  	self assert: method literals second isCompiledBlock 
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: {(#a -> 3)} asDictionary;
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : #'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: {(#MyVar -> 3)} asDictionary;
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]
