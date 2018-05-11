"
I am the entrance door to the ocompletion algorithm. 
I can resolve the queried sequences and retrieve a list of answers according to ocompletion algorithm based on the last accepted sequences from the user.

"
Class {
	#name : #NOCController,
	#superclass : #NECController,
	#category : #'NECompletion-OCompletion'
}

{ #category : #private }
NOCController >> contextClass [
	^NOCContext
]

{ #category : #private }
NOCController >> menuMorphClass [
	^ NOCMenuMorph
]
