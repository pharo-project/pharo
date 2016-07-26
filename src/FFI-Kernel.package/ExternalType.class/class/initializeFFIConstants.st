class initialization
initializeFFIConstants
	"ExternalType initialize"
	FFIConstants initialize. "ensure proper initialization"
	AtomicTypeNames := IdentityDictionary new.
	AtomicSelectors := IdentityDictionary new.
	AtomicTypeNames
		at: FFITypeVoid put: 'void';
		at: FFITypeBool put: 'bool';
		at: FFITypeUnsignedByte put: 'byte';
		at: FFITypeSignedByte put: 'sbyte';
		at: FFITypeUnsignedShort put: 'ushort';
		at: FFITypeSignedShort put: 'short';
		at: FFITypeUnsignedInt put: 'ulong';
		at: FFITypeSignedInt put: 'long';
		at: FFITypeUnsignedLongLong put: 'ulonglong';
		at: FFITypeSignedLongLong put: 'longlong';
		at: FFITypeUnsignedChar put: 'char';
		at: FFITypeSignedChar put: 'schar';
		at: FFITypeSingleFloat put: 'float';
		at: FFITypeDoubleFloat put: 'double';
	yourself.

	AtomicSelectors
		at: FFITypeVoid put: #voidAt:;
		at: FFITypeBool put: #booleanAt:;
		at: FFITypeUnsignedByte put: #unsignedByteAt:;
		at: FFITypeSignedByte put: #signedByteAt:;
		at: FFITypeUnsignedShort put: #unsignedShortAt:;
		at: FFITypeSignedShort put: #signedShortAt:;
		at: FFITypeUnsignedInt put: #unsignedLongAt:;
		at: FFITypeSignedInt put: #signedLongAt:;
		at: FFITypeUnsignedLongLong put: #unsignedLongLongAt:;
		at: FFITypeSignedLongLong put: #signedLongLongAt:;
		at: FFITypeUnsignedChar put: #unsignedCharAt:;
		at: FFITypeSignedChar put: #signedCharAt:;
		at: FFITypeSingleFloat put: #floatAt:;
		at: FFITypeDoubleFloat put: #doubleAt:;
	yourself