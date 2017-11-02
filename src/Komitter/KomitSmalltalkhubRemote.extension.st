Extension { #name : #KomitSmalltalkhubRemote }

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
