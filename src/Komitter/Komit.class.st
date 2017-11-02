"
I am a representation of a commit.

I am abstracted from Monticello to be able to be reused with other frameworks
"
Class {
	#name : #Komit,
	#superclass : #KomitableObject,
	#category : 'Komitter-Models'
}

{ #category : #protocol }
Komit >> commitOn: aSubmitter [

	aSubmitter commitKomit: self
]
