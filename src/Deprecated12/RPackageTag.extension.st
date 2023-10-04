Extension { #name : 'RPackageTag' }

{ #category : '*Deprecated12' }
RPackageTag >> hasClass: aClass [

	self deprecated: 'Use #includesClass: instead' transformWith: '`@rcv hasClass: `@arg' -> '`@rcv includesClass: `@arg'.
	^ self includesClass: aClass
]
