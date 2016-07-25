testing
testPopulate
	| cache data |
	cache := self newCache.
	cache at: #foo put: 1.
	self assert: cache hits isZero.
	self assert: cache misses isZero.
	self assert: cache size equals: 1.
	self assert: (cache at: #foo) equals: 1.
	cache at: #foo put: 2.
	self assert: cache size equals: 1.
	self assert: (cache at: #foo) equals: 2.
	cache := self newCache.
	data := { #x->1. #y->2. #z->3 } asDictionary.
	cache addAll: data.
	self assert: cache hits isZero.
	self assert: cache misses isZero.
	self assert: cache size equals: 3.
	data keysAndValuesDo: [ :key :value |
		self assert: (cache at: key) equals: value ]