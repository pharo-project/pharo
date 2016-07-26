instance creation
newCString: aString "<String> ^<Alien>"
	"Answer an Alien pointing to a null-terminated copy of aString on the external C heap."

	| size cString |
	size := aString size.
	^(Alien rawNewC: size + 1)
		replaceFrom: 1 to: size with: aString startingAt: 1;
		unsignedByteAt: size + 1 put: 0;
		yourself