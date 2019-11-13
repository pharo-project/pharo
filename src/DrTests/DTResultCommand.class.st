"
I am an abstract command concerning DrTests' results tree.
"
Class {
	#name : #DTResultCommand,
	#superclass : #DTCommand,
	#category : #'DrTests-Commands'
}

{ #category : #hooks }
DTResultCommand >> resultSelected [
	^ self drTest resultSelected
]
