"
I am an abstract command concerning DrTests.
"
Class {
	#name : #DTCommand,
	#superclass : #CmCommand,
	#category : #'DrTests-Commands'
}

{ #category : #accessing }
DTCommand >> drTest [
	^ self context
]

{ #category : #accessing }
DTCommand >> plugin [
	^ self drTest currentPlugin
]
