*System-CachingTests
testSupportGetCachedAssociations
	| result |
	result := OrderedCollection new.
	self keysAndValuesDo: [ :key :value |
		result add: (key -> value)
		].
	^result asArray.