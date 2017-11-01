"
Raises an error that represents that the compilation has been aborted. If not handled, it signals an UnhandledError.
"
Class {
	#name : #OCAbortCompilation,
	#superclass : #Error,
	#instVars : [
		'sourceCode'
	],
	#category : 'OpalCompiler-Core-Exception'
}

{ #category : #accessing }
OCAbortCompilation class >> sourceCode: code [
	^ self new
		sourceCode: code;
		yourself
]

{ #category : #accessing }
OCAbortCompilation >> sourceCode [
	^ sourceCode
]

{ #category : #accessing }
OCAbortCompilation >> sourceCode: anObject [
	sourceCode := anObject
]
