tests - DictionaryIndexAccessing
testAtPutNil
	"self run: #testAtPutNil"

	"self debug: #testAtPutNil"

	| collection keyIn newValue |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.
	newValue := self valueNotIn.
	self assert: (collection at: nil put: newValue) == newValue.
	self supportsNilKey
		ifTrue: [ self assert: (collection at: nil) == newValue ]
		ifFalse: [ self deny: (collection includesKey: nil) ].
	self assert: (collection at: keyIn put: nil) isNil.
	self assert: (collection at: keyIn) isNil.
	self assert: (collection at: self keyNotIn put: nil) isNil.
	self assert: (collection at: self keyNotIn) isNil.
	self assert: (collection at: nil put: nil) isNil.
	self supportsNilKey
		ifTrue: [ self assert: (collection at: nil) isNil ]
		ifFalse: [ self deny: (collection includesKey: nil) ]