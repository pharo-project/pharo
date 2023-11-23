Extension { #name : 'RGMethod' }

{ #category : '*Deprecated12' }
RGMethod >> isDefinedInPackage: aPackage [

	self
		deprecated: 'This method will be removed because it is really specific and easy to inline. Use `receiver package = argument` instead.'
		transformWith: '`@rcv isDefinedInPackage: `@package' -> '`@rcv package = `@package'.
	^ self package = aPackage
]
