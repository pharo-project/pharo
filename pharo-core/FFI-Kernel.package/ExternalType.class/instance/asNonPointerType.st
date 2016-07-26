converting
asNonPointerType
	"convert the receiver into a non pointer type"
	self isPointerType
		ifTrue:[^referencedType]
		ifFalse:[^self]