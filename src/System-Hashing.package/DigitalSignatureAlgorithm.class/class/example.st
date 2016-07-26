examples
example
	"Example of signing a message and verifying its signature."
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm example"

	| msg keys sig |
	msg := 'This is a test...'.
	keys := self testKeySet.
	sig := self sign: msg privateKey: keys first.
	self inform: 'Signature created'.
	(self verify: sig isSignatureOf: msg publicKey: keys last)
		ifTrue: [self inform: 'Signature verified.']
		ifFalse: [self error: 'ERROR! Signature verification failed'].
