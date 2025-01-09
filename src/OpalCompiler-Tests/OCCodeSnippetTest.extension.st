Extension { #name : 'OCCodeSnippetTest' }

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileFailBlock [

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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileFaulty [

	| method |

	self skipIf: #compile.
	method := snippet compile.
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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileOnError [

	| method error |
	error := nil.
	method := snippet compileOnError: [ :e |
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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileOnErrorResume [

	| method error |
	error := nil.
	method := snippet compileOnError: [ :e |
		          self assert:
			          (snippet hasNotice: e messageText at: e position).
		          error := e.
		          e resume ].
	self assert: snippet isFaulty equals: error isNotNil.
	self assert: method isCompiledMethod.
	self testExecute: method
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileUndeclaredFaultyFailBlock [

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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testCompileWithRequestor [

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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testDoSemanticAnalysis [
	"We should test more than that"

	| ast |
	ast := snippet doSemanticAnalysis.
	self assert: ast isMethod.
	self assert: ast isFaulty equals: snippet isFaulty.

	self assert: (snippet hasAllNotices: ast allNotices)
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testDoSemanticAnalysisOnError [
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

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testDump [

	| ast dump ast2 dump2 |
	ast := snippet parse.
	dump := ast dump.
	ast2 := OpalCompiler new evaluate: dump.
	self assert: ast2 equals: ast.
	dump2 := ast2 dump.
	self assert: dump2 equals: dump
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testExecute: method [

	| runBlock phonyArgs |
	self skipIf: #exec.
	self assert: method isCompiledMethod.

	phonyArgs := (1 to: method numArgs) asArray.
	runBlock := [ nil withArgs: phonyArgs executeMethod: method ].

	self testExecuteBlock: runBlock
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippetTest >> testExecuteBlock: aRunBlock [

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
