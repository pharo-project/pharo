private
asByteArrayPointer
	"Answer a ByteArray containing a copy of pointer to the contents of the receiver."
	| sz |
	^(ByteArray basicNew: (sz := self class wordSize))
		replaceFrom: 1 to: sz with: self startingAt: 1 "answers self"