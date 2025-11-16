Extension { #name : 'SmallDictionary' }

{ #category : '*Deprecated14' }
SmallDictionary >> bindingsDo: aBlock [

	self
		deprecated:
		'use classVariables do: instead'
		transformWith: '`@receiver bindingsDo: `@arg1' -> '`@receiver associationsDo: `@arg1'.

	self associationsDo: aBlock
]
