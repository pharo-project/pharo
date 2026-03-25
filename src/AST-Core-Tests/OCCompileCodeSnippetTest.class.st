Class {
	#name : 'OCCompileCodeSnippetTest',
	#superclass : 'OCCodeSnippetTest',
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'helpers' }
OCCompileCodeSnippetTest >> compileSnippet: anOCCodeSnippet [ 

	^ [ OpalCompiler new
		  permitFaulty: true;
		  isScripting: anOCCodeSnippet isScripting;
		  compile: anOCCodeSnippet source ]
	on: OCCodeError do: [ :e |
		"Compilation should success, because its the *faulty* mode".
		"If this is expected, then just return nil"
		anOCCodeSnippet ifSkip: #compile then: [^ nil ].
		"Otherwise, pass the error"
		e pass ]
]

{ #category : 'helpers' }
OCCompileCodeSnippetTest >> compileSnippet: aSnippet onError: errorBlock [

	^ [
	  OpalCompiler new
		  isScripting: aSnippet isScripting;
		  compile: aSnippet source ]
		  on: OCCodeError
		  do: [ :e | errorBlock cull: e ]
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileFailBlock [

	| method error |
	error := nil.
	method := OpalCompiler new
		          isScripting: snippet isScripting;
		          failBlock: [ :e |
			          self assert: (snippet hasNotice: e).
			          self assert: error isNil. "single invocation"
			          error := e.
			          #tag ];
		          compile: snippet source.

	snippet isFaulty
		ifTrue: [
			self assert: error isNotNil.
			self assert: method equals: #tag ]
		ifFalse: [
			self assert: error isNil.
			self assert: method isCompiledMethod.
			self testExecute: method ]
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileFaulty [

	| method |

	self skipIf: #compile.
	method := self compileSnippet: snippet.
	self assert: method isCompiledMethod.

	snippet isFaulty
		ifTrue: [
			| ast |
			self assert: method hasSourceCode.
			self assert: method sourceCode equals: snippet source.
			ast := method ast.
			self assert: ast isFaulty.

			"Some faulty AST can produce non faulty method where no `signalSyntaxError:` is present"
			]
		ifFalse: [
			self deny: method isFaulty ].
	self testExecute: method
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileOnError [

	| method error |
	error := nil.
	method := self compileSnippet: snippet onError: [ :e |
		          self assert:
			          (snippet hasNotice: e messageText at: e position).
		          self assert: error isNil. "single invocation"
		          error := e ].
	snippet isFaulty
		ifTrue: [ self assert: error isNotNil ]
		ifFalse: [
			self assert: error isNil.
			self assert: method isCompiledMethod.
			self testExecute: method ]
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileOnErrorResume [

	| method error |
	error := nil.
	method := self compileSnippet: snippet onError: [ :e |
		          self assert:
			          (snippet hasNotice: e messageText at: e position).
		          error := e.
		          e resume ].
	self assert: snippet isFaulty equals: error isNotNil.
	self assert: method isCompiledMethod.
	self testExecute: method
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileUndeclaredFaultyFailBlock [

	| method error |
	error := nil.
	method := OpalCompiler new
		          isScripting: snippet isScripting;
		          permitUndeclared: true;
		          failBlock: [ :e |
			          self assert: (snippet hasNotice: e).
			          self assert: error isNil. "single invocation"
			          error := e.
			          #tag ];
		          compile: snippet source.

	snippet isFaultyMinusUndeclared
		ifTrue: [
			self assert: error isNotNil.
			self assert: method equals: #tag ]
		ifFalse: [
			self assert: error isNil.
			self assert: method isCompiledMethod.
			self testExecute: method ]
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testCompileWithRequestor [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor interactive: false.
	requestor isScripting: nil.
	requestor text: nil.
	method := OpalCompiler new
		          isScripting: snippet isScripting;
		          requestor: requestor;
		          failBlock: [ "When a requestion is set, a failBlock MUST also be set or compilation might crash internally"
			          | n |
			          self assert: requestor notifyList size equals: 1.
			          n := requestor notifyList first.
			          self assert:
					          (snippet
						           hasNotice: (n first allButLast: 3)
						           at: n second).
			          self assert: snippet isFaulty.
			          ^ self ];
		          compile: snippet source.

	"Still alive? (failBlock never called)"
	self deny: snippet isFaulty.
	self assert: requestor notifyList isEmpty.
	self assert: method isCompiledMethod.
	self testExecute: method
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testDecompile [

	| method ast |
	method := self compileSnippet: snippet.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ast := method decompile.
	self assert: ast isMethod.
	ast := method parseTree.
	self assert: ast isMethod.
	"Decompilation lose many information. Not sure how to test more"
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testDecompileIR [

	| method ir |
	method := self compileSnippet: snippet.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ir := method decompileIR.
	self assert: ir class equals: OCIRMethod.
	"Decompilation lose information. Not sure how to test more"
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testDoSemanticAnalysis [
	"We should test more than that"

	| ast |
	ast := snippet doSemanticAnalysis.
	self assert: ast isMethod.
	self assert: ast isFaulty equals: snippet isFaulty.

	self assert: (snippet hasAllNotices: ast allNotices)
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testDoSemanticAnalysisOnError [
	"We should test more than that"

	| ast error |
	error := nil.

	ast := snippet doSemanticAnalysisOnError: [ :e | error := e messageText ].

	snippet isFaulty
		ifTrue: [ self assert: error isNotNil ]
		ifFalse: [
			self deny: ast isFaulty.
			self assert: error isNil ]
]

{ #category : 'tests' }
OCCompileCodeSnippetTest >> testDump [

	| ast dump ast2 dump2 |
	ast := snippet parse.
	dump := ast dump.
	ast2 := OpalCompiler new evaluate: dump.
	self assert: ast2 equals: ast.
	dump2 := ast2 dump.
	self assert: dump2 equals: dump
]

{ #category : 'helpers' }
OCCompileCodeSnippetTest >> testExecute: method [

	| runBlock phonyArgs |
	self skipIf: #exec.
	self assert: method isCompiledMethod.

	phonyArgs := (1 to: method numArgs) asArray.
	runBlock := [ nil withArgs: phonyArgs executeMethod: method ].

	self testExecuteBlock: runBlock
]

{ #category : 'helpers' }
OCCompileCodeSnippetTest >> testExecuteBlock: aRunBlock [

	| runBlock |
	"a block that apply value on aRunBlock until it's no more a block"
	runBlock := [
	            | value block phonyBlockArgs |
	            block := aRunBlock.
	            [  phonyBlockArgs := (1 to: block numArgs) asArray.
	               value := block valueWithArguments: phonyBlockArgs.
	               value isBlock ] whileTrue: [ block := value ].
	            value ].

	"Now we can evaluate and check the various possible expectations"
	snippet messageNotUnderstood ifNotNil: [ :mnu |
		runBlock onDNU: mnu do: [ ^ self ].
		self signalFailure: 'Should have raised MNU ' , mnu ].

	snippet raise ifNotNil: [ :class |
		^ self should: runBlock raise: class ].

	snippet hasValue
		ifFalse: [ self should: runBlock raise: OCRuntimeSyntaxError ]
		ifTrue: [ self assert: runBlock value equals: snippet value ]
]
