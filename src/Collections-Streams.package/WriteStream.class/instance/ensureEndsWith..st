accessing
ensureEndsWith: anObject
	"Append anObject to the receiver IFF it is non-empty and there is not one on the end."

	(position == 0 or: [(collection at: position) = anObject]) ifTrue: [^self].
	self nextPut: anObject