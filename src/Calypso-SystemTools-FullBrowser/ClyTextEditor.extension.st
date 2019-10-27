Extension { #name : #ClyTextEditor }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyTextEditor >> browseFullClass [
	"self lineSelectAndEmptyCheck: [^ self]."
	
	self evaluateSelectionAndDo: [:result | 
		self browser browseClassNamed: result class instanceSide name ]
]
