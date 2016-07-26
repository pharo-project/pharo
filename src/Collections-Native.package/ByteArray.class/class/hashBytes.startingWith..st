byte based hash
hashBytes: aByteArray startingWith: speciesHash
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	<var: #aHash declareC: 'int speciesHash'>
	<var: #aByteArray declareC: 'unsigned char *aByteArray'>

	byteArraySize := aByteArray size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [:pos |
		hash := hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash