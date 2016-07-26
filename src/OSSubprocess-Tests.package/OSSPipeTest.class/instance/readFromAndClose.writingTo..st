private
readFromAndClose: aPipe writingTo: aStream

	| s |
	[aPipe atEndOfFile] whileFalse:
		[s := aPipe next: 10000.
		aStream nextPutAll: s asString.
		(Delay forMilliseconds: 100) wait].
	(aPipe respondsTo: #reader) ifTrue: [aPipe reader close].
	^ aStream
