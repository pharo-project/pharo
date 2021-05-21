"
I am an abstract command concerning DrTests.
"
Class {
	#name : #DTCommand,
	#superclass : #CmCommand,
	#category : #'DrTests-Commands'
}

{ #category : #testing }
DTCommand class >> isAbstract [ 

	^self name = #DTCommand
]

{ #category : #accessing }
DTCommand >> plugin [
	^ self context plugin
]
