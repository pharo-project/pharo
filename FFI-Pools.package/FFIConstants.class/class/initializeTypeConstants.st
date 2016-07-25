pool initialization
initializeTypeConstants
	"type void"
	FFITypeVoid := 0.

	"type bool"
	FFITypeBool := 1.

	"basic integer types.
	note: (integerType anyMask: 1) = integerType isSigned"

	FFITypeUnsignedByte := 2.
	FFITypeSignedByte := 3.
	FFITypeUnsignedShort := 4.
	FFITypeSignedShort := 5.
	FFITypeUnsignedInt := 6.
	FFITypeSignedInt := 7.

	"64bit types"
	FFITypeUnsignedLongLong := 8.
	FFITypeSignedLongLong := 9.

	"special integer types"
	FFITypeUnsignedChar := 10.
	FFITypeSignedChar := 11.

	"float types"
	FFITypeSingleFloat := 12.
	FFITypeDoubleFloat := 13.

	"type flags"
	FFIFlagAtomic := 16r40000. "type is atomic"
	FFIFlagPointer := 16r20000. "type is pointer to base type"
	FFIFlagStructure := 16r10000. "baseType is structure of 64k length"
	FFIStructSizeMask := 16rFFFF. "mask for max size of structure"
	FFIAtomicTypeMask := 16r0F000000. "mask for atomic type spec"
	FFIAtomicTypeShift := 24. "shift for atomic type"
