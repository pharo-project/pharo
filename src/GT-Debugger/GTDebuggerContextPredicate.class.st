"
I encapsulate a condition that can be verified against a Context.

To add concrete conditions a subclass should be create that overrides the method #matchContext:

Public API and Key Messages
- #matches: 
- #hasMatched
- #result

Instance Variables
result:		the last value returned by matches:


"
Class {
	#name : #GTDebuggerContextPredicate,
	#superclass : #Object,
	#instVars : [
		'result'
	],
	#category : #'GT-Debugger-Model'
}

{ #category : #testing }
GTDebuggerContextPredicate >> hasMatched [

	^ self result ifNil: [ ^ false ]
]

{ #category : #testing }
GTDebuggerContextPredicate >> matchContext: aContext [
	"Subclasses should implement this method to add concrete conditions. "
	
	self subclassResponsibility
]

{ #category : #testing }
GTDebuggerContextPredicate >> matches: aContext [
	"Return true if the given context satisfies the condition contained by this predicate,
	or false otherwise."
	
	^ result := self matchContext: aContext
]

{ #category : #accessing }
GTDebuggerContextPredicate >> result [
	"The value returned by the most recent call to matches: "
	
	^ result
]
