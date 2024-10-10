Extension { #name : 'ASTCodeSnippetScriptingTest' }

{ #category : '*OpalCompiler-Tests' }
ASTCodeSnippetScriptingTest >> testEvaluateFailBlock [

	| runBlock error |
	self skipIf: #exec.

	error := nil.
	runBlock := [
		OpalCompiler new
			failBlock: [ :e | error := e. #tag ];
			evaluate: snippet source ].

	snippet isFaulty
		ifTrue: [
			self assert: runBlock value equals: #tag.
			self assert: (snippet hasNotice: error) ]
		ifFalse: [
			self assert: error isNil.
			self testExecuteBlock: runBlock ]
]

{ #category : '*OpalCompiler-Tests' }
ASTCodeSnippetScriptingTest >> testEvaluateFaulty [

	| runBlock |
	self skipIf: #exec.

	runBlock := [
		OpalCompiler new
			permitFaulty: true;
			evaluate: snippet source ].

	self testExecuteBlock: runBlock
]

{ #category : '*OpalCompiler-Tests' }
ASTCodeSnippetScriptingTest >> testEvaluateOnError [

	| runBlock value |
	self skipIf: #exec.

	value := #tag.
	runBlock := [ value := OpalCompiler new evaluate: snippet source ].

	snippet isFaulty
		ifTrue: [
			self should: runBlock raise: CodeError.
			self assert: value equals: #tag ]
		ifFalse: [ self testExecuteBlock: runBlock ]
]

{ #category : '*OpalCompiler-Tests' }
ASTCodeSnippetScriptingTest >> testEvaluateOnErrorResume [

	| runBlock value |
	self skipIf: #exec.

	value := #tag.
	runBlock := [
	            [ OpalCompiler new evaluate: snippet source ]
		            on: CodeError
		            do: [ :e | e resume ] ].

	self testExecuteBlock: runBlock
]
