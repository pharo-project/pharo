tests
testWideString

	| anObject |
	anObject := OrderedCollection new.
	600 timesRepeat: [
	       anObject add: (WideString
	               streamContents: [ :stream |
	                       600 timesRepeat: [
	                               stream nextPut: (256 to: 1000) atRandom asCharacter ] ] ) ].
	
	self
		shouldnt: [ self assertSerializationEqualityOf: anObject ]
		raise: Error