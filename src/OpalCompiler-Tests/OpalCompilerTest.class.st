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
	result := OpalEvaluator new
		bindings: {(#UndefinedObject -> Object)};
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayBindingsWriteGlobals [
	| result |
	result := OpalEvaluator new
		 bindings: {(#Object -> Point)};
       evaluate: 'Object := 42'.
	self assert: result equals: 42
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindings [
	| result |
	result := OpalEvaluator new
		bindings: {(#a -> 3)};
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindingsReadGlobal [
| result |
	result := OpalEvaluator new
	bindings: {(#a -> 3)};
	evaluate: 'OrderedCollection'.
	self assert: result equals: OrderedCollection
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testArrayEvaluateWithBindingsWithUppercaseName [
	| result |
	result := OpalEvaluator new
		bindings: {(#MyVar -> 3)};
		evaluate: '1+MyVar'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testBindingsWithUppercaseNameDoOverwriteGlobals [
	| result |
	result := OpalEvaluator new
		bindings: {(#UndefinedObject -> Object)} asDictionary;
		evaluate: 'UndefinedObject class'.
	self assert: result equals: Object class
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testBindingsWriteGlobals [
	| result |
	result := OpalEvaluator new
		 bindings: {(#Object -> Point)} asDictionary;
       evaluate: 'Object := 42'.
	self assert: result equals: 42
]

{ #category : 'tests' }
OpalCompilerTest >> testCompileSource [
	| source result |
	source := 'tt ^3+4'.

	"Compiled but uninstalled methods have a #source but no sourcePointer"
	result := MockForCompilation compiler source: source; compileOrFail.
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
	method := OpalCompiler new source: 'tst 1+2'; compileOrFail.
	self assert: method methodClass equals: UndefinedObject
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testCompilerUsingCleanBlockClosureHasBlockAsLiteral [

	| compiler method |
	compiler := OpalCompiler new.
	compiler compilationContext compilerOptions:
		#(+ optionCleanBlockClosure ).

	method := compiler source: 'test #(1 2 3) do: [:e | e + 1]'; compileOrFail.

	self assert: method literals second isEmbeddedBlock.
	self assert: method literals second class equals: CleanBlockClosure
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindings [
	| result |
	result := OpalEvaluator new
		bindings: {(#a -> 3)} asDictionary;
		evaluate: '1+a'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindingsSelfSuperThisContext [
	| result |

	"via #bindings: we can overwrite even self, super and thisContext"
	result := OpalEvaluator new
		bindings: {(#self -> 3)} asDictionary;
		evaluate: '1+self'.
	self assert: result equals: 4.

	result := OpalEvaluator new
		bindings: {(#super -> 3)} asDictionary;
		evaluate: '1+super'.
	self assert: result equals: 4.

	result := OpalEvaluator new
		bindings: {(#thisContext -> 3)} asDictionary;
		evaluate: '1+thisContext'.
	self assert: result equals: 4.
	
	result := OpalEvaluator new
		bindings: {(#thisProcess -> 3)} asDictionary;
		evaluate: '1+thisProcess'.
	self assert: result equals: 4
]

{ #category : 'tests - bindings' }
OpalCompilerTest >> testEvaluateWithBindingsWithUppercaseName [
	| result |
	result := OpalEvaluator new
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
		          changeStamp: '2025-04-25T14:42:06.014486+02:00';
		          protocol: 'hitching';
		          install.

	self assert: method equals: MockForCompilation >> #foo.
	self assert: MockForCompilation new foo equals: 42.

	"Cleanup"
	method removeFromSystem.
	self deny: (MockForCompilation includesSelector: #foo)
]

{ #category : 'tests' }
OpalCompilerTest >> testSavedProtocolInChangesFileIsRight [
	"Regression test, the saved protocol was wrong when we provided a Protocol instance during compilation.."
	
	| method |
	"Cleanup"
	(MockForCompilation includesSelector: #foo) ifTrue: [
		MockForCompilation removeSelector: #foo ].
	"Precond"
	self deny: (MockForCompilation includesSelector: #foo).

	method := MockForCompilation compiler
		          source: 'foo ^42';
		          protocol: (Protocol named: 'hitching');
		          install.

	self assert: (SourcesManager current protocolAt: method sourcePointer) equals: 'hitching'.
	"Cleanup"
	method removeFromSystem.
	self deny: (MockForCompilation includesSelector: #foo)
]

{ #category : 'tests' }
OpalCompilerTest >> testShadowPseudoVariable [
	"We can shadw PseudoVariables (here thisContext) and it works"
	| result |
	result := OpalEvaluator new
       evaluate: '| thisContext | thisContext := 42. ^thisContext'.
	self assert: result equals: 42
]
