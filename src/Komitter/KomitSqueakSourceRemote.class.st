"
I encapsulate a repository stored on squeaksource
"
Class {
	#name : #KomitSqueakSourceRemote,
	#superclass : #KomitRemote,
	#category : #'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitSqueakSourceRemote >> icon [
	^ self iconNamed: #komitterSqueakSourceRemote
]

{ #category : #'*Komitter-UI' }
KomitSqueakSourceRemote >> label [
	| url |
	url := self remote location splitOn: '/'.
	
	^ String streamContents: [ :stream | stream << (url at: 4) ]
]
