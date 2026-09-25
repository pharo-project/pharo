Extension { #name : 'SharedQueue' }

{ #category : '*TaskIt' }
SharedQueue >> fixedSize: aSize nextPut: anObject [

	self monitor critical: [
			[ aSize - self items size < 1 ] whileTrue: [ self items removeFirst ].
			self items addLast: anObject.
			self monitor signal ].
	^ anObject
]
