Class {
	#name : 'OpalCompilerTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayBindingsWithUppercaseNameDoOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: {(#UndefinedObject -> Object)};
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayBindingsWriteGlobals [
	| result |
	result := Smalltalk compiler
		 bindings: {(#Object -> Point)};
       evaluate: 'Object := 42'.
	self assert: result equals: 42
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: {(#a -> 3)};
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindingsReadGlobal [
| result |
	result := Smalltalk compiler
	bindings: {(#a -> 3)};
	evaluate: 'OrderedCollection'.
	self assert: result equals: OrderedCollection
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: {(#MyVar -> 3)};
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testBindingsWithUppercaseNameDoOverwriteGlobals [
	| result |
	result := Smalltalk compiler
		bindings: {(#UndefinedObject -> Object)} asDictionary;
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testBindingsWriteGlobals [
	| result |
	result := Smalltalk compiler
		 bindings: {(#Object -> Point)} asDictionary;
       evaluate: 'Object := 42'.
	self assert: result equals: 42
]

{ #category : 'tests' }
OpalCompilerTest >> testCompileFromAnalysedAST [

	| source ast value |
	source := 'foo |a| a := 3. ^a+4'.
	
	ast := MockForCompilation compiler parse: source.
	value := MockForCompilation compiler ast: ast; evaluate.
	self assert: value equals: 7.
]

{ #category : 'tests' }
OpalCompilerTest >> testCompileFromText [

	| source result |
	source := 'tt ^3+4' asText.

	result := MockForCompilation compiler compile: source.
	self assert: result sourceCode isString.
	self assert: result sourceCode equals: source asString
]

{ #category : 'tests' }
OpalCompilerTest >> testCompileSource [
	| source result |
	source := 'tt ^3+4'.

	"Compiled but uninstalled methods have a #source but no sourcePointer"
	result := MockForCompilation compiler compile: source.
	self assert: (result valueWithReceiver: nil) equals: 7.
	self deny: result hasSourcePointer. "no sourcePointer"
	self assert: (result propertyAt: #source) equals: source.
	self assert: result sourceCodeOrNil equals: source.

	"Installed methods have asourcePointer but no #source"
	result := MockForCompilation compiler install: source.
	self assert: result equals: MockForCompilation>>#tt.
	self assert: (result valueWithReceiver: nil) equals: 7.
	self assert: result hasSourcePointer. "no sourcePointer"
	self assert: (result propertyAt: #source) equals: nil.
	self assert: result sourceCodeOrNil equals: source.

	"When uninstalled, they still keep their source pointer"
	MockForCompilation compiler install: 'tt ^42'.
	self deny: result equals: MockForCompilation>>#tt.
	self assert: (result valueWithReceiver: nil) equals: 7.
	self assert: result hasSourcePointer. "no sourcePointer"
	self assert: (result propertyAt: #source) equals: nil.
	self assert: result sourceCodeOrNil equals: source.
	
	(MockForCompilation>>#tt) removeFromSystem
]

{ #category : 'tests' }
OpalCompilerTest >> testCompileWithNilClass [
	"we shoud use UndefinedObject if the class is nil"
	| method |
	method := Smalltalk compiler compile: 'tst 1+2'.
	self assert: method methodClass equals: UndefinedObject
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testCompilerUsingCleanBlockClosureHasBlockAsLiteral [

	| compiler method |
	compiler := Smalltalk compiler.
	compiler compilationContext compilerOptions:
		#(+ optionCleanBlockClosure ).

	method := compiler compile: 'test #(1 2 3) do: [:e | e + 1]'.

	self assert: method literals second isEmbeddedBlock.
	self assert: method literals second class equals: CleanBlockClosure
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindings [
	| result |
	result := Smalltalk compiler
		bindings: {(#a -> 3)} asDictionary;
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindingsSelfSuperThisContext [
	| result |

	"via #bindings: we can overwrite even self, super and thisContext"
	result := Smalltalk compiler
		bindings: {(#self -> 3)} asDictionary;
		evaluate: '1+self'.
	self assert: result equals: 4.

	result := Smalltalk compiler
		bindings: {(#super -> 3)} asDictionary;
		evaluate: '1+super'.
	self assert: result equals: 4.

	result := Smalltalk compiler
		bindings: {(#thisContext -> 3)} asDictionary;
		evaluate: '1+thisContext'.
	self assert: result equals: 4.
	
	result := Smalltalk compiler
		bindings: {(#thisProcess -> 3)} asDictionary;
		evaluate: '1+thisProcess'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindingsWithUppercaseName [
	| result |
	result := Smalltalk compiler
		bindings: {(#MyVar -> 3)} asDictionary;
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]

{ #category : 'tests' }
OpalCompilerTest >> testInstall [

	| method |
	"Cleanup"
	(MockForCompilation includesSelector: #foo) ifTrue: [
		MockForCompilation removeSelector: #foo ].
	"Precond"
	self deny: (MockForCompilation includesSelector: #foo).

	method := MockForCompilation compiler
		          source: 'foo ^42';
		          changeStamp: 'JP 4/01/2023 01:23';
		          protocol: 'hitching';
		          install.

	self assert: method equals: MockForCompilation >> #foo.
	self assert: MockForCompilation new foo equals: 42.

	"Cleanup"
	method removeFromSystem.
	self deny: (MockForCompilation includesSelector: #foo)
]

{ #category : 'tests' }
OpalCompilerTest >> testInstallException [

	| method message |
	"Precond"
	self deny: (MockForCompilation includesSelector: #foo).

	[ method := MockForCompilation compiler install: 'foo ^¿' ]
		on: OCCodeError
		do: [ :error | message := error messageText , ' :(' ].

	self deny: (MockForCompilation includesSelector: #foo).
	self assert: method isNil.
	self assert: message equals: 'Unknown character :('
]

{ #category : 'tests' }
OpalCompilerTest >> testInstallRequestor [

	| method requestor |
	"precond"
	self deny: (MockForCompilation includesSelector: #foo).

	requestor := OCMockRequestor new.

	[
	method := MockForCompilation compiler
		          source: 'foo ^¿';
		          requestor: requestor;
		          failBlock: [  ];
		          changeStamp: 'JP 3/24/2023 18:39';
		          protocol: 'hitching';
		          install ]
		on: OCCodeError
		do: [  ].

	self deny: (MockForCompilation includesSelector: #foo).
	self assert: method isNil.
	self assert: requestor notifyList first first equals: 'Unknown character ->'
]

{ #category : 'tests' }
OpalCompilerTest >> testShadowPseudoVariable [
	"We can shadw PseudoVariables (here thisContext) and it works"
	| result |
	result := Smalltalk compiler
       evaluate: '| thisContext | thisContext := 42. ^thisContext'.
	self assert: result equals: 42
]
