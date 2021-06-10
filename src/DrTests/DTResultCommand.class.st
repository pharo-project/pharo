"
I am an abstract command concerning DrTests' results tree.
"
Class {
	#name : #DTResultCommand,
	#superclass : #DTCommand,
	#category : #'DrTests-Commands'
}

{ #category : #testing }
DTResultCommand class >> isAbstract [ 

	^ self name = #DTResultCommand
]

{ #category : #hooks }
DTResultCommand >> resultSelected [
	^ self context resultSelected
]
