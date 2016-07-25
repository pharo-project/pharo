examples
exampleSprintf
	"Call the libc sprintf function."
	"UnsafeAlien exampleSprintf"
	| r s |
	(Alien lookup: 'sprintf' inLibrary: Alien libcName)
		primFFICallResult: (r := Alien new: 4)
		withArguments: {self forPointerTo: (s := ByteString new: 1024).
						 self forPointerTo: 'Hello World %d %x !!', (String with: Character lf).
						 123.
						 48879}.
	^s copyFrom: 1 to: (r signedLongAt: 1) - 1 "trim the null byte"