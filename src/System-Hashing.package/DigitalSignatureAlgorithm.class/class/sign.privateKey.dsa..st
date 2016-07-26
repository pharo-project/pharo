public
sign: aStringOrStream privateKey: privateKey dsa: dsa 
	"Sign the given message (a stream or string) and answer a signature string."
	"Note: Unguessable random numbers are needed for message signing. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before signing a message. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."
	| hasher h sig |
	hasher := SHA1 new.
	h := aStringOrStream class isBytes 
		ifTrue: [ hasher hashMessage: aStringOrStream ]
		ifFalse: [ hasher hashStream: aStringOrStream ].
	sig := dsa 
		computeSignatureForMessageHash: h
		privateKey: privateKey.
	^ dsa signatureToString: sig