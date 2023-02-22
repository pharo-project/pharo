Extension { #name : #RBCodeSnippetTest }

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testCompile [

	| method |
	method := snippet compile.
	self assert: method isCompiledMethod
]

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippetTest >> testExecute [

	| method runBlock |
	self skipIf: #testExecute.
	method := snippet compile.
	self assert: method isCompiledMethod.

	"Executing a lone block will just return a block, we have to call value to have something more interesting"
	snippet parse isBlock
		ifTrue: [
			| block |
			block := nil executeMethod: method.
			runBlock := [ block cull: 1 cull: 2 ] ]
		ifFalse: [ runBlock := [ nil executeMethod: method ] ].

	snippet messageNotUnderstood ifNotNil: [ :mnu |
		runBlock onDNU: mnu do: [ ^ self ].
		self signalFailure: 'Should have raised MNU ' , mnu ].

	snippet isFaulty
		ifTrue: [ self should: runBlock raise: RuntimeSyntaxError ]
		ifFalse: [ self assert: runBlock value equals: snippet value ]
]
