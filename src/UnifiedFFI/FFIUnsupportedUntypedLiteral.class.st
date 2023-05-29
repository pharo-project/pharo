"
I am an error indicating that an unsupported untyped literal has been used.
I contain the unsupported literal so the user can query me.
"
Class {
	#name : #FFIUnsupportedUntypedLiteral,
	#superclass : #Error,
	#instVars : [
		'literal'
	],
	#category : #'UnifiedFFI-Exceptions'
}

{ #category : #signalling }
FFIUnsupportedUntypedLiteral class >> signalFor: aLiteral [

	self new
		literal: aLiteral;
		signal
]

{ #category : #accessing }
FFIUnsupportedUntypedLiteral >> literal [
	^ literal
]

{ #category : #accessing }
FFIUnsupportedUntypedLiteral >> literal: anObject [
	literal := anObject
]
