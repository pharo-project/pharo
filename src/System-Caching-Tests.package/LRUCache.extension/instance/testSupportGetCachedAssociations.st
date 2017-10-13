*System-Caching-Tests
testSupportGetCachedAssociations
	| result |
	result := OrderedCollection new.
	self keysAndValuesDo: [ :key :value |
		result add: (key -> value)
		].
	^result asArray.
