tests
testNilHashCollision
	"Ensures that fixCollisionsFrom: does the right thing in the presence of a nil key"
	| dict key |
	self supportsNilKey
		ifFalse: [ ^ self ].
	dict := self collectionClass new.
	key := nil hash. "any key with same hash as nil"
	dict at: key hash put: 1.
	dict at: nil put: 2.
	self assert: (dict includesKey: nil).
	dict removeKey: key.
	self assert: (dict includesKey: nil).
