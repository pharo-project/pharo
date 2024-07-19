Extension { #name : 'Character' }

{ #category : '*Deprecated12' }
Character class >> allCharacters [

	"This name is obsolete since only the characters that will fit in a byte can be queried"

	self
		deprecated:
			'This name is obsolete since only the characters that will fit in a byte can be queried'
		transformWith: '`@rec allCharacters' -> '`@rec allByteCharacters'.

	^ self allByteCharacters
]
