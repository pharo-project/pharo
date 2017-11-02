Extension { #name : #LRUCache }

{ #category : #'*System-Caching-Tests' }
LRUCache >> testSupportGetCachedAssociations [
	| result |
	result := OrderedCollection new.
	self keysAndValuesDo: [ :key :value |
		result add: (key -> value)
		].
	^result asArray.

]
