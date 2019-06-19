Extension { #name : #StringMorph }

{ #category : #'*Calypso-Browser' }
StringMorph >> clyAddEmphasis: aTextEmphasis [
	"For compatibility between Pharo 6 and Pharo 7 and to not produce overrides"
	self emphasis: (emphasis bitOr: aTextEmphasis emphasisCode)
]
