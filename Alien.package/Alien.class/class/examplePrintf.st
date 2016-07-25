examples
examplePrintf
	"Call the libc printf function."
	"Alien examplePrintf"
	| r s |
	(self lookup: 'printf' inLibrary: self libcName)
		primFFICallResult: (r := Alien new: 4)
		withArguments: {(s := ('Hello World %d %x !!', (String with: Character lf)) asAlien) pointer.
						 123.
						 48879}.
	s free.
	^r signedLongAt: 1