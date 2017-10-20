tests
testFindLastOccurenceOfStringStartingAt

	self assert: ('Smalltalk' findLastOccurrenceOfString: 'al' startingAt: 2) = 7.
	self assert: ('aaa' findLastOccurrenceOfString: 'aa' startingAt: 1) = 2.
	self assert: ('Smalltalk' asWideString findLastOccurrenceOfString: 'al' startingAt: 2) = 7.
	self assert: ('Smalltalk' asWideString findLastOccurrenceOfString: 'al' asWideString startingAt: 2) = 7.
	self assert: (('Smalltalk' copyWith: 835 asCharacter) findLastOccurrenceOfString: 'al' asWideString startingAt: 2) = 7.