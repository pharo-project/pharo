"
When compiling syntactically incorrect code, we compile raising this exception.

This way the debugger opens and the programmer can easily fix the faulty method
"
Class {
	#name : #RuntimeSyntaxError,
	#superclass : #Error,
	#instVars : [
		'errorNode'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signalling }
RuntimeSyntaxError class >> signal: aNode [
	^(self new errorNode: aNode) signal
]

{ #category : #accessing }
RuntimeSyntaxError >> errorMessage [
	^errorNode errorMessage
]

{ #category : #accessing }
RuntimeSyntaxError >> errorNode: aNode [
	errorNode := aNode
]

{ #category : #accessing }
RuntimeSyntaxError >> messageText [
	^self errorMessage
]
