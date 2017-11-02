"
I encapsulate a gemstone repository
"
Class {
	#name : #KomitHttpRemote,
	#superclass : #KomitRemote,
	#category : 'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitHttpRemote >> icon [

	^ 'HTTP'
]

{ #category : #'*Komitter-UI' }
KomitHttpRemote >> label [
	
	^ self remote location
]
