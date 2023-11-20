Extension { #name : 'PackageTag' }

{ #category : '*Deprecated12' }
PackageTag >> hasClass: aClass [

	self deprecated: 'Use #includesClass: instead' transformWith: '`@rcv hasClass: `@arg' -> '`@rcv includesClass: `@arg'.
	^ self includesClass: aClass
]
