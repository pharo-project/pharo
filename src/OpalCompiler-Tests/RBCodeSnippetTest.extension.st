Extension { #name : #RBCodeSnippetTest }

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testCompile [

	| method |

	self skipIf: #compile.
	method := snippet compile.
	self assert: method isCompiledMethod
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testCompileOnError [

	| method error |
	error := nil.
	method := snippet compileOnError: [ :e | error := e errorMessage ].
	snippet isFaulty
		ifTrue: [ self assert: error isNotNil ]
		ifFalse: [
			self assert: error isNil.
			self assert: method isCompiledMethod ]
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testDecompile [

	| method ast |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ast := method decompile.
	self assert: ast isMethod.
	"Decompilation lose many information. Not sure how to test more"
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testDecompileIR [

	| method ir |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ir := method decompileIR.
	self assert: ir class equals: IRMethod.
	"Decompilation lose information. Not sure how to test more"
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testDoSemanticAnalysis [
	"We should test more than that"

	| ast |
	ast := snippet doSemanticAnalysis.
	self assert: ast isMethod.
	"self assert: ast isFaulty equals: snippet isFaulty" self flag: #FIXME
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testDoSemanticAnalysisOnError [
	"We should test more than that"

	| ast error |
	error := nil.

	ast := snippet doSemanticAnalysisOnError: [ :e | error := e errorMessage ].

	snippet isFaulty
		ifTrue: [ self assert: error isNotNil ]
		ifFalse: [
			self deny: ast isFaulty.
			self assert: error isNil ]
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testExecute [

	| method runBlock phonyArgs |
	self skipIf: #testExecute.
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Compilation error is for an other test"
	self assert: method isCompiledMethod.

	phonyArgs := (1 to: method numArgs) asArray.

	"Executing a lone block will just return a block, we have to call value to have something more interesting"
	snippet parse isBlock
		ifTrue: [
			| block phonyBlockArgs |
			block := nil withArgs: phonyArgs executeMethod: method.
			phonyBlockArgs := (1 to: block numArgs) asArray.
			runBlock := [ block valueWithArguments: phonyBlockArgs ] ]
		ifFalse: [ runBlock := [ nil withArgs: phonyArgs executeMethod: method ] ].

	snippet messageNotUnderstood ifNotNil: [ :mnu |
		runBlock onDNU: mnu do: [ ^ self ].
		self signalFailure: 'Should have raised MNU ' , mnu ].

	snippet raise ifNotNil: [ :class | ^ self should: runBlock raise: class ].

	snippet hasValue
		ifFalse: [ self should: runBlock raise: RuntimeSyntaxError ]
		ifTrue: [ self assert: runBlock value equals: snippet value ]
]
