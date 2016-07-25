testing
testEmpty
	| emptyCache |
	emptyCache := self newCache.
	self assert: emptyCache isEmpty.
	self assert: emptyCache size isZero.
	self assert: emptyCache hits isZero.
	self assert: emptyCache misses isZero.
	self assert: emptyCache hitRatio isZero.
	self assert: emptyCache totalWeight isZero.
	self deny: (emptyCache includesKey: #foo).
	emptyCache keysAndValuesDo: [ :key :value | self fail ]