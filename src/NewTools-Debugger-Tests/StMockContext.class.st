"
I'm an object answering to part of the API of Context, but doing nothing.
You can use me in your tests where messages are sent to a context but you do not actually want to act on a real one, or when you want to record what was done on the context.
"
Class {
	#name : #StMockContext,
	#superclass : #Object,
	#instVars : [
		'stepIntoQuickMethodBooleans'
	],
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #'debugger access' }
StMockContext >> stepIntoQuickMethod: aBoolean [
	self stepIntoQuickMethodBooleans add: aBoolean
]

{ #category : #accessing }
StMockContext >> stepIntoQuickMethodBooleans [

	^ stepIntoQuickMethodBooleans ifNil: [ 
		  stepIntoQuickMethodBooleans := OrderedCollection new ]
]
