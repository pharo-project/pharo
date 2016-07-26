actions
obfuscateIfNecessary: anObject
	^ Privacy sendEntityNames
			ifTrue: [ anObject ]
			ifFalse: [ self obfuscate: anObject ]