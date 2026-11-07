Class {
	#name : 'OpalCompilerTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'tests - array' }
OpalCompilerTest >> arrayElements: aCount [

	^ ' . ' join: ((1 to: aCount) collect: [ :each | each asString ])
]

{ #category : 'tests - array' }
OpalCompilerTest >> arraySourceWithTemps: tempCount pending: pendingCount nesting: nestingCount [
	"A method with many temps and a large array whose last element nests sends, as in issue 19058"

	| nested |
	nested := (1 to: nestingCount) inject: '1' into: [ :source :each | '(1 max: ' , source , ')' ].
	^ String streamContents: [ :stream |
			  stream nextPutAll: 'test | '.
			  1 to: tempCount do: [ :index | stream nextPutAll: 't' , index asString , ' ' ].
			  stream nextPutAll: '| ^ { '.
			  1 to: pendingCount do: [ :index | stream nextPutAll: index asString , ' . ' ].
			  stream nextPutAll: nested , ' }' ]
]

{ #category : 'tests - array' }
OpalCompilerTest >> assertConsArrayCompilationOf: aString equals: anArray translator: aTranslatorClass [

	| method |
	method := self compileArraySource: aString options: #( #+ optionPushConsArray ) translator: aTranslatorClass.
	self deny: (method sendsSelector: #new:).
	self deny: (method sendsSelector: #at:put:).
	self assert: (nil executeMethod: method) equals: anArray
]

{ #category : 'tests - array' }
OpalCompilerTest >> assertLargeArrayCompilationOf: aString equals: anArray translator: aTranslatorClass [

	| method |
	method := self compileArraySource: aString options: #( #- optionPushConsArray ) translator: aTranslatorClass.
	self assert: (method sendsSelector: #new:).
	self assert: (nil executeMethod: method) equals: anArray
]

{ #category : 'tests - array' }
OpalCompilerTest >> compileArraySource: aString options: anArray translator: aTranslatorClass [

	| compiler |
	compiler := OpalCompiler new.
	compiler compilationContext
		compilerOptions: anArray;
		astTranslatorClass: aTranslatorClass.
	^ compiler
		  source: aString;
		  compileOrFail
]

{ #category : 'tests - array' }
OpalCompilerTest >> inlinedTempsSource: aCount [

	^ String streamContents: [ :stream |
			  stream nextPutAll: '| '.
			  1 to: aCount do: [ :index | stream nextPutAll: 'x' , index asString , ' ' ].
			  stream nextPutAll: '| '.
			  1 to: aCount do: [ :index | stream nextPutAll: 'x' , index asString , ' := 1. ' ] ]
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayAfterInlinedBlockCountsItsTemps [

	| method |
	method := self
		          compileArraySource:
		          'test true ifTrue: [ ' , (self inlinedTempsSource: 20) , ' ]. ^ { ' , (self arrayElements: 40) , ' }'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (method sendsSelector: #at:put:).
	self assert: (nil executeMethod: method) size equals: 40
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayAsArgumentCountsPendingStack [

	| method |
	method := self
		          compileArraySource: 'test ^ 1 max: (2 max: (3 max: ((1 to: 50) size max: { ' , (self arrayElements: 50) , ' } size)))'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (nil executeMethod: method) equals: 50
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayBeforeInlinedBlockCountsItsTemps [

	| method |
	method := self
		          compileArraySource:
		          'test | r | r := { ' , (self arrayElements: 40) , ' }. true ifTrue: [ ' , (self inlinedTempsSource: 20) , ' ]. ^ r'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (method sendsSelector: #at:put:).
	self assert: (nil executeMethod: method) size equals: 40
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayBeforeInlinedLoopCountsItsTemps [

	| method |
	method := self
		          compileArraySource:
		          'test | r | r := { ' , (self arrayElements: 40) , ' }. 1 to: 1 do: [ :i | ' , (self inlinedTempsSource: 20)
		          , ' ]. ^ r'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (method sendsSelector: #at:put:).
	self assert: (nil executeMethod: method) size equals: 40
]

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

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayInFullBlockCountsBlockTemps [

	| method |
	method := self
		          compileArraySource: 'test ^ [ ' , (self inlinedTempsSource: 30) , ' { ' , (self arrayElements: 30) , ' } ] value'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (nil executeMethod: method) size equals: 30
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayWithNestedElementFittingFrameUsesConsArray [

	| method |
	method := self
		          compileArraySource: (self arraySourceWithTemps: 0 pending: 20 nesting: 5)
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self deny: (method sendsSelector: #at:put:)
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayWithNestedElementOverflowingFrameFallsBackToLargeArray [

	| method |
	method := self
		          compileArraySource: (self arraySourceWithTemps: 30 pending: 20 nesting: 10)
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (method sendsSelector: #at:put:).
	self assert: (nil executeMethod: method) size equals: 21
]

{ #category : 'tests - array' }
OpalCompilerTest >> testArrayWithNestedElementOverflowingFrameInReflectivity [

	| method |
	method := self
		          compileArraySource: (self arraySourceWithTemps: 30 pending: 20 nesting: 10)
		          options: #( #+ optionPushConsArray )
		          translator: RFASTTranslator.
	self assert: (nil executeMethod: method) size equals: 21
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

{ #category : 'tests - array' }
OpalCompilerTest >> testElementOverflowingFrameByItselfStillFails [

	self
		should: [
				self
					compileArraySource: (self arraySourceWithTemps: 0 pending: 1 nesting: 60)
					options: #( #+ optionPushConsArray )
					translator: OCASTTranslator ]
		raise: Error
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

{ #category : 'tests - array' }
OpalCompilerTest >> testMaxFrameSizeDecidesArrayCompilation [

	| compile |
	compile := [ :size |
		           | compiler |
		           compiler := OpalCompiler new.
		           compiler compilationContext
			           compilerOptions: #( #+ optionPushConsArray );
			           maxFrameSize: size.
		           compiler
			           source: 'test ^ { 1 . 2 . 3 . 4 }';
			           compileOrFail ].
	self deny: ((compile value: 4) sendsSelector: #at:put:).
	self assert: ((compile value: 3) sendsSelector: #at:put:)
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOptionPushConsArrayDisabledUsesLargeArray [

	self assertLargeArrayCompilationOf: 'test ^ { }' equals: #(  ) translator: OCASTTranslator.
	self assertLargeArrayCompilationOf: 'test ^ { 1 }' equals: #( 1 ) translator: OCASTTranslator.
	self assertLargeArrayCompilationOf: 'test ^ { 1 . 2 + 1 }' equals: #( 1 3 ) translator: OCASTTranslator
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOptionPushConsArrayDisabledUsesLargeArrayInReflectivity [

	self assertLargeArrayCompilationOf: 'test ^ { }' equals: #(  ) translator: RFASTTranslator.
	self assertLargeArrayCompilationOf: 'test ^ { 1 . 2 + 1 }' equals: #( 1 3 ) translator: RFASTTranslator
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOptionPushConsArrayEnabledUsesConsArray [

	self assertConsArrayCompilationOf: 'test ^ { }' equals: #(  ) translator: OCASTTranslator.
	self assertConsArrayCompilationOf: 'test ^ { 1 }' equals: #( 1 ) translator: OCASTTranslator.
	self assertConsArrayCompilationOf: 'test ^ { 1 . 2 + 1 }' equals: #( 1 3 ) translator: OCASTTranslator
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOptionPushConsArrayEnabledUsesConsArrayInReflectivity [

	self assertConsArrayCompilationOf: 'test ^ { 1 . 2 + 1 }' equals: #( 1 3 ) translator: RFASTTranslator
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOverflowingArrayKeepsDeepOneElementNestingAsConsArray [

	| nested method |
	nested := (1 to: 20) inject: '1' into: [ :source :each | '{ ' , source , ' }' ].
	method := self
		          compileArraySource: 'test ^ { ' , (self arrayElements: 56) , ' . ' , nested , ' }'
		          options: #( #+ optionPushConsArray )
		          translator: OCASTTranslator.
	self assert: (nil executeMethod: method) size equals: 57
]

{ #category : 'tests - array' }
OpalCompilerTest >> testOverflowingArrayKeepsInnerArrayAsConsArray [

	| source consFirst allLarge |
	source := 'test ^ { ' , (self arrayElements: 50) , ' . { ' , (self arrayElements: 10) , ' } }'.
	consFirst := self compileArraySource: source options: #( #+ optionPushConsArray ) translator: OCASTTranslator.
	allLarge := self compileArraySource: source options: #( #- optionPushConsArray ) translator: OCASTTranslator.
	self assert: (consFirst sendsSelector: #at:put:).
	self assert: consFirst bytecodes size < allLarge bytecodes size.
	self assert: (nil executeMethod: consFirst) equals: (nil executeMethod: allLarge)
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
