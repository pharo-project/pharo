character writing
ensureASpace
	"Append a space character to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = Character space]) ifTrue: [^self].
	self nextPut: Character space