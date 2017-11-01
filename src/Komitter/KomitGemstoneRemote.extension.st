Extension { #name : #KomitGemstoneRemote }

{ #category : #'*Komitter-UI' }
KomitGemstoneRemote >> icon [
	^ self iconNamed: #komitterGemstoneRemote
]

{ #category : #'*Komitter-UI' }
KomitGemstoneRemote >> label [
	| url |
	url := self remote location splitOn: '/'.
	
	^ String streamContents: [ :stream | stream << (url at: 5) ]
]
