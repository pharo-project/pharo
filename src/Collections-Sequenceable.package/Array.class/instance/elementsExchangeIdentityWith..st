converting
elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation."

	<primitive: 128 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[Smalltalk garbageCollect < 1048576 ifTrue:
			[Smalltalk growMemoryByAtLeast: 1048576].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed