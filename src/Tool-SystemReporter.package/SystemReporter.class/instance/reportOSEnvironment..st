reporting
reportOSEnvironment: aStream
	| env |
	self header: 'Operating System Environment' on: aStream.
	env := [Smalltalk os environment] on: Error do: [^self].
	env keys asSortedCollection do: [:name |
		aStream nextPutAll: name; nextPut: $=; nextPutAll: (env at: name); cr]
	