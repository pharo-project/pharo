Extension { #name : 'OrderedDictionary' }

{ #category : '*Deprecated12' }
OrderedDictionary >> bindingsDo: aBlock [

	self
		deprecated:
		'use classVariables do: instead'
		transformWith: '`@receiver bindingsDo: `@arg1' -> '`@receiver associationsDo: `@arg1'.

	self associationsDo: aBlock
]
