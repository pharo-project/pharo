"
I am an error indicating that an unsupported function declaration is being parsed.
I contain the unsupported node so the user can query me
"
Class {
	#name : #FFIUnsupportedFunctionDeclarationError,
	#superclass : #Error,
	#instVars : [
		'node'
	],
	#category : #'UnifiedFFI-Exceptions'
}

{ #category : #signalling }
FFIUnsupportedFunctionDeclarationError class >> signalFor: anRBProgramNode [

	self new
		node: anRBProgramNode;
		signal
]

{ #category : #accessing }
FFIUnsupportedFunctionDeclarationError >> node [
	^ node
]

{ #category : #accessing }
FFIUnsupportedFunctionDeclarationError >> node: anObject [
	node := anObject
]
