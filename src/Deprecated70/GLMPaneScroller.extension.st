Extension { #name : #GLMPaneScroller }

{ #category : #'*Deprecated70' }
GLMPaneScroller >> initialColorInSystemWindow: aSystemWindow [
	"Answer the colour the receiver should be when added to a SystemWindow."
	self deprecated: 'Deprecated without replacement.' on: '2018-03-16' in: #Pharo7.
	^Color transparent
]
