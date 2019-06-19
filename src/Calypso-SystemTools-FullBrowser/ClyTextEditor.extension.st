Extension { #name : #ClyTextEditor }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyTextEditor >> browseIt [
	"self lineSelectAndEmptyCheck: [^ self]."
	self browser browseClassNamed: (self findClassFromAST ifNil: [ ^self ])
]
