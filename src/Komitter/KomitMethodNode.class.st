"
I am a node in the Komit tree representing a method
"
Class {
	#name : #KomitMethodNode,
	#superclass : #KomitLeaf,
	#category : #Komitter-UI
}

{ #category : #api }
KomitMethodNode >> label [

	^ self content selector
]
