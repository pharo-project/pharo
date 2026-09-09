Extension { #name : 'RubScrolledTextModel' }

{ #category : '*Deprecated14' }
RubScrolledTextModel >> text: atextOrString [

	self
		deprecated: 'You should use setText: or privateText: if you are really in the same class'
		transformWith: '`@receiver text: `@arg' -> '`@receiver setText: `@arg'.
		
			
	^ self setText: atextOrString
]
