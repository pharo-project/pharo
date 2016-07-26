An external type represents the type of external objects.

Instance variables:
	compiledSpec	<WordArray>		Compiled specification of the external type
	referentClass	<Behavior | nil>	Class type of argument required
	referencedType	<ExternalType>	Associated (non)pointer type with the receiver

Compiled Spec:
The compiled spec defines the type in terms which are understood by the VM. Each word is defined as:
	bits 0...15 	- byte size of the entity
	bit 16		- structure flag (FFIFlagStructure)
				  This flag is set if the following words define a structure
	bit 17		- pointer flag (FFIFlagPointer)
				  This flag is set if the entity represents a pointer to another object
	bit 18		- atomic flag (FFIFlagAtomic)
				  This flag is set if the entity represents an atomic type.
				  If the flag is set the atomic type bits are valid.
	bits 19...23	- unused
	bits 24...27	- atomic type (FFITypeVoid ... FFITypeDoubleFloat)
	bits 28...31	- unused

Note that all combinations of the flags FFIFlagPointer, FFIFlagAtomic, and FFIFlagStructure are invalid, EXCEPT from the following:

	FFIFlagPointer + FFIFlagAtomic:
		This defines a pointer to an atomic type (e.g., 'char*', 'int*').
		The actual atomic type is represented in the atomic type bits.

	FFIFlagPointer + FFIFlagStructure:
		This defines a structure which is a typedef of a pointer type as in
			typedef void* VoidPointer;
			typedef Pixmap* PixmapPtr;
		It requires a byte size of four (e.g. a 32bit pointer) to work correctly.

[Note: Other combinations may be allowed in the future]
