"
I decide when the SUnit debugger should be active.
"
Class {
	#name : #GTSUnitActivationPredicate,
	#superclass : #GTDebuggerContextPredicate,
	#category : #'GT-SUnitDebugger'
}

{ #category : #testing }
GTSUnitActivationPredicate class >> isDeprecated [
	^ true
]

{ #category : #testing }
GTSUnitActivationPredicate >> matchContext: aContext [
	
	^ aContext method isTestMethod
]
