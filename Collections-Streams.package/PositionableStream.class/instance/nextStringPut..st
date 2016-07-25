nonhomogeneous accessing
nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length := s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s