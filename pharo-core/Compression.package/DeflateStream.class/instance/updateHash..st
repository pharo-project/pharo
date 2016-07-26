deflating
updateHash: nextValue
	"Update the running hash value based on the next input byte.
	Return the new updated hash value."
	^((hashValue bitShift: HashShift) bitXor: nextValue) bitAnd: HashMask.