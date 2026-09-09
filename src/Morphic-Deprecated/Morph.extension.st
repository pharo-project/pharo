Extension { #name : 'Morph' }

{ #category : '*Morphic-Deprecated' }
Morph >> on: aShortcut do: anAction [

	self
		deprecated: 'Use bindKeyCombination:toAction:'
		on: '16/11/2024'
		in: 'pharo13'
		transformWith: '`@receiver on: `@aShortcut do: `@anAction'
						-> '`@receiver bindKeyCombination: `@aShortcut toAction: `@anAction'.
						
	^ self bindKeyCombination: aShortcut toAction: anAction
	


]
