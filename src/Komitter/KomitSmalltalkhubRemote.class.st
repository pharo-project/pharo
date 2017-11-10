"
I encapsulate a repository stored on smalltalkhub
"
Class {
	#name : #KomitSmalltalkhubRemote,
	#superclass : #KomitRemote,
	#category : #Komitter-Models
}

{ #category : #'*Komitter-UI' }
KomitSmalltalkhubRemote >> icon [
	^ self iconNamed: #komitterSmalltalkhubRemote
]

{ #category : #'*Komitter-UI' }
KomitSmalltalkhubRemote >> label [
	| url |
	url := self remote location splitOn: '/'.
	
	^ String streamContents: [ :stream | stream << (url at: 6) << ' / ' << (url at: 5) ]
]
