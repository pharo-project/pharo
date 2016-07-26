printing
printAtomicType: spec on: aStream
	self assert: (spec anyMask: FFIFlagAtomic).
	aStream nextPutAll: (#(	'void' 'unsigned char' 'unsigned char' 'signed char'
							'unsigned short' 'short' 'unsigned long' 'long'
							'unsigned long long' 'long long' 'char' 'signed char'
							'float' 'double') at: ((spec bitAnd: FFIAtomicTypeMask) bitShift: FFIAtomicTypeShift negated) + 1).
	aStream space.
	(spec anyMask: FFIFlagPointer) ifTrue:
		[aStream nextPut: $*]