tests
testKeysAndValuesDoWithGarbageCollectedValue

	self dictionaryWithGarbageCollectedValue keysAndValuesDo: [:key :value | self fail ]
