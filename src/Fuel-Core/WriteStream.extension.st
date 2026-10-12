Extension { #name : 'WriteStream' }

{ #category : '*Fuel-Core' }
WriteStream >> nextBytesPutAll: aCollection [
	"Append the bytes of aCollection to the sequence of bytes accessible 
	by the receiver. Answer aCollection."

	| newEnd |
	self collection class instSpec == aCollection class instSpec ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd := self position + aCollection size.
	newEnd > self writeLimit ifTrue: [ self growTo: newEnd + 10 ].

	self collection
		replaceFrom: self position + 1
		to: newEnd
		with: aCollection
		startingAt: 1.
	self position: newEnd.

	^ aCollection
]
