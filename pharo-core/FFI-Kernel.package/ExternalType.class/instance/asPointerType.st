converting
asPointerType
	"convert the receiver into a pointer type"
	self isPointerType
		ifTrue:[^self]
		ifFalse:[^referencedType]