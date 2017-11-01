"
I encapsulate a gemstone repository
"
Class {
	#name : #KomitDirectoryRemote,
	#superclass : #KomitRemote,
	#category : 'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitDirectoryRemote >> icon [
	^ 'DIR'
]

{ #category : #'*Komitter-UI' }
KomitDirectoryRemote >> label [
	
	^ self remote directory basename
]
