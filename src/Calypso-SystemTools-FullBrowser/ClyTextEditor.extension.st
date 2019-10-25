Extension { #name : #ClyTextEditor }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyTextEditor >> browseFullClass [
	"self lineSelectAndEmptyCheck: [^ self]."
	self browser browseClassNamed: (self findClassFromAST ifNil: [ ^self ])
]
