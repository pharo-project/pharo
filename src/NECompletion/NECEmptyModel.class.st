"
I am an empty completion Model. I am used if no completion is expected.

For example, typing a message send and using constants (Strings/Numbers) for the arguments,
we don't want to complete the constant arguments with any exisiting symbol containing that constant:
3 + 12 
should not show completion menu with entries like CP1250TextConverter, ExampleForTest12, ...


"
Class {
	#name : #NECEmptyModel,
	#superclass : #NECModel,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECEmptyModel >> initializeSelectors [
	self resetSelectors
]

{ #category : #testing }
NECEmptyModel >> isEmpty [
	^ true
]

{ #category : #action }
NECEmptyModel >> narrowWith: aString [
	"no results"
]
