examples
timeDecode: count
	"Example of signing a message and verifying its signature."
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm timeDecode: 20"

	| dsa |

	dsa := self new.
	dsa initRandomFromUser.

	#(1 10 100 1000 10000 100000) do: [ :extraLen | | s msg keys sig |
		s := String new: extraLen.
		1 to: s size do: [ :i | s at: i put: (Character value: 200 atRandom)].
		msg := 'This is a test...',s.
		keys := self testKeySet.
		sig := self sign: msg privateKey: keys first dsa: dsa.
		"self inform: 'Signature created'."
		self timeDirect: [
			count timesRepeat: [
				(self verify: sig isSignatureOf: msg publicKey: keys last)
					ifFalse: [self error: 'ERROR! Signature verification failed'].
			].
		] as: 'verify msgLen = ',msg size printString count: count
	].
