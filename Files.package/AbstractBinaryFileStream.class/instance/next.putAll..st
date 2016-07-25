writing
next: amount putAll: aByteArray

	forWrite ifFalse: [^ self error: 'Cannot write a read-only file'].
	self primWrite: handle from: aByteArray startingAt: 1 count: amount.
	^ aByteArray