"
I am a class used to access some of the main classes of Keymapping. See my class side to have an idea of what can I give you.
"
Class {
	#name : #KMFactory,
	#superclass : #Object,
	#category : #'Keymapping-Core-Base'
}

{ #category : #accessing }
KMFactory class >> keymapContainer [
	^ KMRepository default
]
