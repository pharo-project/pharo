Extension { #name : #SycNotRemoveMethodStrategy }

{ #category : #'*Calypso-SystemTools-Core' }
SycNotRemoveMethodStrategy >> orderForBrowser [
	"Cancelling the remove of command should be at the end of list"
	^super orderForBrowser + 1.
]
