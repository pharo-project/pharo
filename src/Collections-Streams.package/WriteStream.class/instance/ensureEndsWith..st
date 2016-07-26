accessing
ensureEndsWith: anObject
	"Append anObject to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = anObject]) ifTrue: [^self].
	self nextPut: anObject