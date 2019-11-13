Extension { #name : #DigitalSignatureAlgorithm }

{ #category : #'*System-Hashing-Testing' }
DigitalSignatureAlgorithm class >> testExamplesFromDisk [
	"verify messages from file on disk"

	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."

	"DigitalSignatureAlgorithm testExamplesFromDisk"

	FileStream
		readOnlyFileNamed: 'dsa.test.out'
		do: [ :file | 
			| msg sig publicKey |
			[ file atEnd ]
				whileFalse: [ sig := file nextChunk.
					msg := file nextChunk.
					publicKey := self class compiler evaluate: file nextChunk.
					self
						assert: (self verify: sig isSignatureOf: msg publicKey: publicKey)
						description: 'ERROR! Signature verification failed' ] ]
]

{ #category : #'*System-Hashing-Testing' }
DigitalSignatureAlgorithm class >> writeExamplesToDisk [
	"Example of signing a message and verifying its signature. Used to create samples from one implementation that could later be tested with a different implementation"
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm writeExamplesToDisk"

	| file keyList dsa msgList |

	dsa := self new.
	dsa initRandomFromUser.
	self inform: 'About to generate 5 key sets. Will take a while'.
	keyList := {self testKeySet},((1 to: 5) collect: [ :ignore | self generateKeySet]).
	msgList := {'This is a test...'. 'This is the second test period.'. 'And finally, a third message'}.
	file := FileStream newFileNamed: 'dsa.test.out'.
	[
		msgList do: [ :msg |
			keyList do: [ :keys | | sig |
				sig := self sign: msg privateKey: keys first dsa: dsa.
				(self verify: sig isSignatureOf: msg publicKey: keys last) ifTrue: [
					file
						nextChunkPut: sig;
						nextChunkPut: msg;
						nextChunkPut: keys last storeString.
				] ifFalse: [
					self error: 'ERROR! Signature verification failed'
				].
			].
		].
	] ensure: [file close]

]
