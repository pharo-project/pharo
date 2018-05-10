Extension { #name : #Morph }

{ #category : #'*Deprecated70' }
Morph >> initialColorInSystemWindow: aSystemWindow [
	"Answer the colour the receiver should be when added to a SystemWindow."
	self deprecated: 'Deprecated without replacement.' on: '2018-03-16' in: #Pharo7.
	^Color white
]
