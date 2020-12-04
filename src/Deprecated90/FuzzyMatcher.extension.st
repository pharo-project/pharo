Extension { #name : #FuzzyMatcher }

{ #category : #'*Deprecated90' }
FuzzyMatcher >> isSeperator: aCharacter [

	self
		deprecated: 'Use isSeparator: now.'
		transformWith: '`@receiver isSeperator: `@aCharacter'
			-> '`@receiver isSeparator: `@aCharacter'.
			
	^  self isSeparator: aCharacter
]
