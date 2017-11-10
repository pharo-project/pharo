Extension { #name : #KomitSqueakSourceRemote }

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
