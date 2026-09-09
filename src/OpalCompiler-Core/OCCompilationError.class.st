Class {
	#name : 'OCCompilationError',
	#superclass : 'Error',
	#instVars : [
		'compilationResult'
	],
	#category : 'OpalCompiler-Core-Exceptions',
	#package : 'OpalCompiler-Core',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
OCCompilationError >> compilationResult [

	^ compilationResult
]

{ #category : 'accessing' }
OCCompilationError >> compilationResult: anObject [

	compilationResult := anObject
]

{ #category : 'initialization' }
OCCompilationError >> initialize [

	super initialize.
	messageText := self messageText
]

{ #category : 'accessing' }
OCCompilationError >> messageText [

	| ast notices res |
	ast := compilationResult ifNotNil: [ compilationResult ast ].
	ast ifNil: [ ^ super messageText ].

	notices := ast allNotices.
	notices ifEmpty: [ ^ super messageText ].

	res := (notices collect: [ :each | '- ' , each asString ]) joinUsing: OSPlatform current lineEnding.
	^ 'Compilation failed with following errors:' , OSPlatform current lineEnding , res
]
