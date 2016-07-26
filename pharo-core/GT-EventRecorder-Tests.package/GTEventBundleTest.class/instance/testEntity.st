tests
testEntity
	bundle data: data.
	entity := bundle entity.
	self assert: (entity isKindOf: ZnByteArrayEntity).
	self assert: entity bytes equals: data.