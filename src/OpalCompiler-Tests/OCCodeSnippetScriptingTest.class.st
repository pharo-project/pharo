Class {
	#name : 'OCCodeSnippetScriptingTest',
	#superclass : 'OCCompileCodeSnippetTest',
	#category : 'OpalCompiler-Tests-Snippets',
	#package : 'OpalCompiler-Tests',
	#tag : 'Snippets'
}

{ #category : 'tests' }
OCCodeSnippetScriptingTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: (OCCodeSnippet allSnippets select: [:each | each isScripting ]);
		  yourself
]

{ #category : 'tests' }
OCCodeSnippetScriptingTest >> testEvaluateFailBlock [

	| runBlock error |
	"self skipIf: #exec."

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

{ #category : 'tests' }
OCCodeSnippetScriptingTest >> testEvaluateFaulty [

	| runBlock |
	self skipIf: #exec.

	runBlock := [
		OpalCompiler new
			permitFaulty: true;
			evaluate: snippet source ].

	self testExecuteBlock: runBlock
]

{ #category : 'tests' }
OCCodeSnippetScriptingTest >> testEvaluateOnError [

	| runBlock value |
	self skipIf: #exec.

	value := #tag.
	runBlock := [ value := OpalCompiler new evaluate: snippet source ].

	snippet isFaulty
		ifTrue: [
			self should: runBlock raise: OCCodeError.
			self assert: value equals: #tag ]
		ifFalse: [ self testExecuteBlock: runBlock ]
]

{ #category : 'tests' }
OCCodeSnippetScriptingTest >> testEvaluateOnErrorResume [

	| runBlock value |
	self skipIf: #exec.

	value := #tag.
	runBlock := [
	            [ OpalCompiler new evaluate: snippet source ]
		            on: OCCodeError
		            do: [ :e | e resume ] ].

	self testExecuteBlock: runBlock
]
