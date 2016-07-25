tests - DictionaryIndexAccessing
testAtPutNil
	"self run: #testAtPutNil"
	"self debug: #testAtPutNil"
	
	| aDictionary keyIn new |
	aDictionary := self nonEmpty .
	keyIn := aDictionary keys anyOne.

	new := 'new'.
	self assert: (aDictionary at: nil put: 'new') == new.
	self supportsNilKey
		ifTrue: [self assert: (aDictionary at: nil) == new]
		ifFalse: [self deny: (aDictionary includesKey: nil)].
	
	self assert: (aDictionary at: keyIn put: nil) isNil.
	self assert: (aDictionary at: keyIn) isNil.
	
	self assert: (aDictionary at: self keyNotIn put: nil) isNil.
	self assert: (aDictionary at: self keyNotIn ) isNil.

	self assert: (aDictionary at: nil put: nil) isNil.
	self supportsNilKey
		ifTrue: [self assert: (aDictionary at: nil) isNil]
		ifFalse: [self deny: (aDictionary includesKey: nil)].