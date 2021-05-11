"
I am just a convenience place holder for the results of an assertion failure:
- actual: the actual result of the assertion
- expected: the expected result of the assertion
I am meant to be used when expected and actual results do not satisfy the assertion.
"
Class {
	#name : #StDebuggerAssertionFailureResult,
	#superclass : #Object,
	#instVars : [
		'actual',
		'expected'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerAssertionFailureResult >> actual [

	^ actual
]

{ #category : #accessing }
StDebuggerAssertionFailureResult >> actual: anObject [

	actual := anObject
]

{ #category : #accessing }
StDebuggerAssertionFailureResult >> expected [

	^ expected
]

{ #category : #accessing }
StDebuggerAssertionFailureResult >> expected: anObject [

	expected := anObject
]
