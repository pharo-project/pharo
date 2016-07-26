public
generateKeySet
 	"Generate and answer a key set for code signing. The result is a pair (<private key><public key>). Each key is an array of four large integers. The signer must be sure to record this keys set and must keep the private key secret to prevent someone from forging their signature."
 	"Note: Key generation can take some time. Open a transcript so you can see what's happening and take a coffee break!"
 	"Note: Unguessable random numbers are needed for key generation. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before generating a key set. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."
 	"DigitalSignatureAlgorithm generateKeySet"
 
 	| dsa |
 	dsa := self new.
 	(self confirm: 'Shall I seed the random generator from the current time?')
 		ifTrue: [dsa initRandomNonInteractively]
 		ifFalse: [dsa initRandomFromUser].
 	^ dsa generateKeySet
 