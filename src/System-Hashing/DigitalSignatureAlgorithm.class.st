"
This class implements the Digital Signature Algorithm (DSA) of the U.S. government's ""Digital Signature Standard"" (DSS). The DSA algorithm was proposed in 1991 and became a standard in May 1994. The official description is available as a Federal Information Processing Standards Publication (FIPS PUB 186, May 19, 1994). A companion standard, the Secure Hash Standard, or SHS (FIPS PUB 180-1, April 17, 1995), describes a 160-bit message digest algorithm known as the Secure Hash Algorithm (SHA). This message digest is used to compute the document signature.

Here's how to use it:

  1. The ""signer"" creates a pair of keys. One of these must be kept private. The other may be freely distributed. For example, it could be built into the signature checking code of an application.

  2. When the signer wishes to sign a packet of data (a ""message"") , he uses the secure hash algorithm to create a 160-bit message digest (hash) which is used as the input to DSA. The result of this is a pair of large numbers called a ""signature"" that is attached to the original message.

  3. When someone receives a signed message purported to have come from the signer, they compute the 160-bit hash of the message and pass that, along with the message signature and the signer's public key, to the signature verification algorithm. If the signature checks, then it is virtually guaranteed that the message originated from someone who had the signer's private key. That is, the message is not a forgery and has not been modified since it was signed. For example, if the message contains a program, and the recipient trusts the signer, then the recipient can run the program with the assurance that it won't do anything harmful. (At least, not intentionally. A digital signature is no guarantee against bugs! :->)

The signer must keep the private key secure, since anyone who has the private key can forge the signer's signature on any message they like. As long as the secret key is not stolen, cryptographers believe it to be virtually impossible either to forge a signature, to find a message that matches an existing sigature, or to discover the signer's private key by analyzing message signatures. Knowing the public key (which, for example, could be recovered from an application that had it built in), does not weaken the security at all.

An excellent reference work on digital signatures and cryptography in general is:

  Schneier, Bruce
  ""Applied Cryptography: Protocols, Algorithms, and Source Code in C""
  John Wiley and Sons, 1996.

I used this book as a guide to implementing many of the numerical algorithms required by DSA.

Patents and Export Restrictions:

Many digital signature technologies are patented. DSA is also patented, but the patent is owned by the U.S. government which has made DSA available royalty-free. There is a claim that the government patent infringes on an earlier patent by Schnorr, but the government is requiring the use of DSA, so they apparently believe this claim is not strong enough to be a serious threat to their own patent.

Most cryptography technology, including digital signature technology, requires an export license for it to be distributed outside the U.S. Recent legislation may have relaxed the export license requirements, but it would be prudent to check the current regulations before exporting this code.
"
Class {
	#name : #DigitalSignatureAlgorithm,
	#superclass : #Object,
	#instVars : [
		'randKey',
		'randSeed'
	],
	#classVars : [
		'HighBitOfByte',
		'SmallPrimes'
	],
	#category : #'System-Hashing-DSA'
}

{ #category : #examples }
DigitalSignatureAlgorithm class >> example [
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

]

{ #category : #public }
DigitalSignatureAlgorithm class >> generateKeySet [
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
 
]

{ #category : #initialization }
DigitalSignatureAlgorithm class >> initialize [
	"DigitalSignatureAlgorithm initialize"

	"SmallPrimes is a list of small primes greater than two."
	SmallPrimes := Integer primesUpTo: 2000.
	SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size.

	"HighBitOfByte maps a byte to the index of its top non-zero bit."
	HighBitOfByte := (0 to: 255) collect: [:byte | byte highBit].

]

{ #category : #public }
DigitalSignatureAlgorithm class >> sign: aStringOrStream privateKey: privateKey [ 
	"Sign the given message (a stream or string) and answer a signature string."
	"Note: Unguessable random numbers are needed for message signing. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before signing a message. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."
	| dsa hasher h sig |
	dsa := self new.
	dsa initRandomFromUser.
	hasher := SHA1 new.
	h := aStringOrStream class isBytes 
		ifTrue: [ hasher hashMessage: aStringOrStream ]
		ifFalse: [ hasher hashStream: aStringOrStream ].
	sig := dsa 
		computeSignatureForMessageHash: h
		privateKey: privateKey.
	^ dsa signatureToString: sig
]

{ #category : #public }
DigitalSignatureAlgorithm class >> sign: aStringOrStream privateKey: privateKey dsa: dsa [ 
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
]

{ #category : #examples }
DigitalSignatureAlgorithm class >> testKeySet [
	"Answer a pair of keys for testing. The first key is the private key, the second one is the public key."
	"WARNING: This test key set is public should be used only for testing! In a real application, the user would create a set of keys using generateKeySet and would keep the private key secret."

	^ #(
		(8343811888543852523216773185009428259187948644369498021763210776677854991854533186365944349987509452133156416880596803846631577352387751880552969116768071 1197175832754339660404549606408619548226315875117 1433467472198821951822151391684734233265646022897503720591270330985699984763922266163182803556189497900262038518780931942996381297743579119123094520048965 957348690772296812)
		(8343811888543852523216773185009428259187948644369498021763210776677854991854533186365944349987509452133156416880596803846631577352387751880552969116768071 1197175832754339660404549606408619548226315875117 1433467472198821951822151391684734233265646022897503720591270330985699984763922266163182803556189497900262038518780931942996381297743579119123094520048965 4645213122572190617807944614677917601101008235397095646475699959851618402406173485853587185431290863173614335452934961425661774118334228449202337038283799))

]

{ #category : #testing }
DigitalSignatureAlgorithm class >> time: aBlock as: aString count: anInteger [

	^{anInteger. aString. (Time millisecondsToRun: aBlock)}
]

{ #category : #examples }
DigitalSignatureAlgorithm class >> timeDecode: count [
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

]

{ #category : #testing }
DigitalSignatureAlgorithm class >> timeDirect: aBlock as: aString count: anInteger [
	self
		trace:
			(String
				streamContents: [ :stream | 
					stream
						nextPutAll: anInteger asStringWithCommas;
						nextPutAll: '  ';
						nextPutAll: aString;
						nextPutAll: ' took ';
						nextPutAll: (Time millisecondsToRun: aBlock) asStringWithCommas;
						nextPutAll: ' ms';
						cr ])
]

{ #category : #public }
DigitalSignatureAlgorithm class >> verify: signatureString isSignatureOf: aStringOrStream publicKey: publicKey [ 
	"Answer true if the given signature string signs the given message (a stream or string)."
	"Note: Random numbers are not needed for signature verification; thus, there is no need to call initRandomFromUser before verifying a signature."
	| dsa hasher h sig |
	dsa := self new.
	hasher := SHA1 new.
	h := aStringOrStream class isBytes 
		ifTrue: [ hasher hashMessage: aStringOrStream ]
		ifFalse: [ hasher hashStream: aStringOrStream ].
	sig := dsa stringToSignature: signatureString.
	^ dsa 
		verifySignature: sig
		ofMessageHash: h
		publicKey: publicKey
]

{ #category : #public }
DigitalSignatureAlgorithm >> computeSignatureForMessageHash: hash privateKey: privateKey [
	"Answer the digital signature of the given message hash using the given private key. A signature is a pair of large integers. The private key is an array of four large integers: (p, q, g, x)."

	| p q g x r s k tmp |
	p := privateKey first.
	q := privateKey second.
	g := privateKey third.
	x := privateKey fourth.

	r := s := 0.
	[r = 0 or: [s = 0]] whileTrue: [
		k := self nextRandom160 \\ q.
		r := (g raisedTo: k modulo: p) \\ q.
		tmp := (hash + (x * r)) \\ q.
		s := ((k reciprocalModulo: q) * tmp) \\ q].

	^ Array with: r with: s

]

{ #category : #public }
DigitalSignatureAlgorithm >> generateKeySet [
	"Generate and answer a key set for DSA. The result is a pair (<private key><public key>). Each key is an array of four large integers. The private key is (p, q, g, x); the public one is (p, q, g, y). The signer must be sure to record (p, q, g, x), and must keep x secret to prevent someone from forging their signature."
	"Note: Key generation can take some time. Open a transcript so you can see what's happening and take a coffee break!"

	| qAndPandS q p exp g h x y |
	qAndPandS := self generateQandP.
	"Transcript show: 'Computing g...'."
	q := qAndPandS first.
	p := qAndPandS second.
	exp := (p - 1) / q.
	h := 2.
	[g := h raisedTo: exp modulo: p. g = 1] whileTrue: [h := h + 1].
	"Transcript show: 'done.'; cr.
	Transcript show: 'Computing x and y...'."
	x := self nextRandom160.
	y := g raisedTo: x modulo: p.
	"Transcript show: 'done.'; cr.
	Transcript show: 'Key generation complete!'; cr."
	^ Array
		with: (Array with: p with: q with: g with: x)
		with: (Array with: p with: q with: g with: y).

]

{ #category : #private }
DigitalSignatureAlgorithm >> generateQandP [
	"Generate the two industrial-grade primes, q (160-bits) and p (512-bit) needed to build a key set. Answer the array (q, p, s), where s is the seed that from which q and p were created. This seed is normally discarded, but can be used to verify the key generation process if desired."

	| pBits halfTwoToTheP chunkCount sAndq q twoQ n c w x p s |
	pBits := 512.  "desired size of p in bits"
	halfTwoToTheP := 2 raisedTo: (pBits - 1).
	chunkCount := pBits // 160.

	"Transcript show: 'Searching for primes q and p...'; cr."
	[true] whileTrue: [
		sAndq := self generateSandQ.
		"Transcript show: '  Found a candidate q.'; cr."
		s := sAndq first.
		q := sAndq last.
		twoQ := q bitShift: 1.
		n := 2.
		c := 0.
		[c < 4096] whileTrue: [
			w := self generateRandomLength: pBits s: s n: n.
			x := w + halfTwoToTheP.
			p := (x - ( x \\ twoQ)) + 1.
			p highBit = pBits ifTrue: [
				"Transcript show: '    Testing potential p ', (c + 1) printString, '...'; cr."
				(self isProbablyPrime: p) ifTrue: [
					"Transcript show: '  Found p!'; cr."
					^ Array with: q with: p with: s]].
			n := n + chunkCount + 1.
			c := c + 1]].

]

{ #category : #private }
DigitalSignatureAlgorithm >> generateRandomLength: bitLength s: s n: n [
	"Answer a random number of bitLength bits generated using the secure hash algorithm."

	| sha out count extraBits v |
	sha := SHA1 new.
	out := 0.
	count := (bitLength // 160).
	extraBits := bitLength - (count * 160).
	0 to: count do: [:k |
		v := sha hashInteger: (s + n + k).
		k = count ifTrue: [
			v := v - ((v >> extraBits) << extraBits)].
		out := out bitOr: (v bitShift: (160 * k))].
	^ out

]

{ #category : #private }
DigitalSignatureAlgorithm >> generateSandQ [
	"Generate a 160-bit random seed s and an industrial grade prime q."

	| hasher s sPlusOne u q |
	hasher := SHA1 new.
	[true] whileTrue: [
		s := self nextRandom160.
		sPlusOne := s + 1.
		sPlusOne highBit > 160 ifTrue: [sPlusOne := sPlusOne \\ (2 raisedTo: 160)].
		u := (hasher hashInteger: s) bitXor: (hasher hashInteger: sPlusOne).
		q := u bitOr: ((1 bitShift: 159) bitOr: 1).
		(self isProbablyPrime: q) ifTrue: [^ Array with: s with: q]].

]

{ #category : #initialization }
DigitalSignatureAlgorithm >> initRandom: randomInteger [
	"Initialize the the secure random number generator with the given value. The argument should be a positive integer of up to 512 bits chosen randomly to avoid someone being able to predict the sequence of random values generated."
	"Note: The random generator must be initialized before generating a key set or signature. Signature verification does not require initialization of the random generator."

	randSeed := 16rEFCDAB8998BADCFE10325476C3D2E1F067452301.  "initial seed"
	randKey := randomInteger.
	"Transcript show: 'Random seed: ', randomInteger printString; cr."

]

{ #category : #initialization }
DigitalSignatureAlgorithm >> initRandomFromString: aString [
	"Ask the user to type a long random string and use the result to seed the secure random number generator."

	| s k srcIndex |
	s := aString.
	k := LargePositiveInteger new: (s size min: 64).
	srcIndex := 0.
	k bytesCount to: 1 by: -1 do: [:i |
		k byteAt: i put: (s at: (srcIndex := srcIndex + 1)) asciiValue].
	k := k normalize + (Random new next * 16r7FFFFFFF) asInteger.  "a few additional bits randomness"
	k highBit > 512 ifTrue: [k := k bitShift: k highBit - 512].
	self initRandom: k.

]

{ #category : #initialization }
DigitalSignatureAlgorithm >> initRandomFromUser [
	"Ask the user to type a long random string and use the result to seed the secure random number generator."

	| s |
	s := UIManager default request: 'Enter a long random string to seed the random generator.'.
	s ifNil: [s := '']. 
	^self initRandomFromString: s
]

{ #category : #initialization }
DigitalSignatureAlgorithm >> initRandomNonInteractively [

 	self initRandomFromString: 
 		Time millisecondClockValue printString, 
 		Date today printString, 
 		Smalltalk os platformName printString
]

{ #category : #'large integer arithmetic' }
DigitalSignatureAlgorithm >> inverseOf: x mod: n [
	"Answer the inverse of x modulus n. That is, the integer y such that (x * y) \\ n is 1. Both x and n must be positive, and it is assumed that x < n and that x and n are integers."
	"Details: Use the extended Euclidean algorithm, Schneier, p. 247."

	| v u u1 u2 u3 t1 t2 t3 tmp |
	((x <= 0) or: [n <= 0]) ifTrue: [self error: 'x and n must be greater than zero'].
	x >= n ifTrue: [self error: 'x must be < n'].

	v := x.
	u := n.
	(x even and: [n even]) ifTrue: [self error: 'no inverse'].

	u1 := 1. u2 := 0. u3 := u.
	t1 := v. t2 := u - 1. t3 := v.
	[	[u3 even ifTrue: [
			((u1 odd) or: [u2 odd]) ifTrue: [
				u1 := u1 + v.
				u2 := u2 + u].
			u1 := u1 bitShift: -1.
			u2 := u2 bitShift: -1.
			u3 := u3 bitShift: -1].
		((t3 even) or: [u3 < t3]) ifTrue: [
			tmp := u1. u1 := t1. t1 := tmp.
			tmp := u2. u2 := t2. t2 := tmp.
			tmp := u3. u3 := t3. t3 := tmp].
		u3 even and: [u3 > 0]] whileTrue: ["loop while u3 is even"].

		[((u1 < t1) or: [u2 < t2]) and: [u1 > 0]] whileTrue: [
			u1 := u1 + v.
			u2 := u2 + u].
	
		u1 := u1 - t1.
		u2 := u2 - t2.
		u3 := u3 - t3.
		t3 > 0] whileTrue: ["loop while t3 > 0"].

	[u1 >= v and: [u2 >= u]] whileTrue: [
		u1 := u1 - v.
		u2 := u2 - u].

	u3 = 1 ifFalse: [self error: 'no inverse'].
	^ u - u2

]

{ #category : #'large integer arithmetic' }
DigitalSignatureAlgorithm >> isProbablyPrime: p [
	"Answer true if p is prime with very high probability. Such a number is sometimes called an 'industrial grade prime'--a large number that is so extremely likely to be prime that it can assumed that it actually is prime for all practical purposes. This implementation uses the Rabin-Miller algorithm (Schneier, p. 159)."

	| iterations pMinusOne b m r a j z couldBePrime |
	iterations := 50.	
	"Note: The DSA spec requires >50 iterations; Schneier says 5 are enough (p. 260)"	"quick elimination: check for p divisible by a small prime"
	SmallPrimes
		ifNil: [ 
			"generate list of small primes > 2"
			SmallPrimes := Integer primesUpTo: 2000.
			SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size ].
	SmallPrimes detect: [ :f | p \\ f = 0 ] ifFound: [ :factor | ^ p = factor ].
	pMinusOne := p - 1.
	b := self logOfLargestPowerOfTwoDividing: pMinusOne.
	m := pMinusOne bitShift: b negated.	
	"Assert: pMinusOne = m * (2 raisedTo: b) and m is odd"
	"Transcript show: '      Prime test pass '."
	r := Random new.
	1 to: iterations do: [ :i | 
		"Transcript show: i printString; space."
		a := (r next * 16rFFFFFF) truncated.
		j := 0.
		z := (a raisedTo: m modulo: p) normalize.
		couldBePrime := z = 1.
		[ couldBePrime ]
			whileFalse: [ 
				z = 1
					ifTrue: [ 
						"Transcript show: 'failed!'; cr."
						^ false ].	"not prime"
				z = pMinusOne
					ifTrue: [ couldBePrime := true ]
					ifFalse: [ 
						(j := j + 1) < b
							ifTrue: [ z := z * z \\ p ]
							ifFalse: [ 
								"Transcript show: 'failed!'; cr."
								^ false ] ] ] ].	"not prime"
	"Transcript show: 'passed!'; cr."
	^ true	 "passed all tests; probably prime"
]

{ #category : #private }
DigitalSignatureAlgorithm >> logOfLargestPowerOfTwoDividing: aPositiveInteger [
	"Answer the base-2 log of the largest power of two that divides the given integer. For example, the largest power of two that divides 24 is 8, whose log base-2 is 3. Do this efficiently even when the given number is a large integer. Assume that the given integer is > 0."
	"DigitalSignatureAlgorithm new logOfLargestPowerOfTwoDividing: (32 * 3)"

	^aPositiveInteger lowBit - 1
]

{ #category : #private }
DigitalSignatureAlgorithm >> nextRandom160 [
	"Answer a newly generated 160-bit random number in the range [1..(2^160 - 1)]."
	"Details: Try again in the extremely unlikely chance that zero is encountered."

	| result |
	result := 0.
	[result = 0] whileTrue: [
		result := SHA1 new hashInteger: randKey seed: randSeed.
		randKey := randKey + result + 1].
	^ result

]

{ #category : #public }
DigitalSignatureAlgorithm >> signatureToString: aSignature [
	"Answer a string representation of the given signature. This string can be parsed using the stringToSignature: method."

	| s |
	s := (String new: 2000) writeStream.
	s nextPutAll: '[DSA digital signature '.
	s nextPutAll: aSignature first printStringHex.
	s space.
	s nextPutAll: aSignature second printStringHex.
	s nextPutAll: ']'.
	^ s contents

]

{ #category : #public }
DigitalSignatureAlgorithm >> stringToSignature: aString [ 
	"Answer the signature stored in the given string. A signature string has the format:

		 '[DSA digital signature <r> <s>]'

	where <r> and <s> are large positive integers represented by strings of hexidecimal digits."
	| prefix stream r s |
	prefix := '[DSA digital signature '.
	(aString beginsWith: prefix) ifFalse: [ self error: 'bad signature prefix' ].
	stream := aString readStream.
	stream position: prefix size.
	r := Integer 
		readFrom: stream
		base: 16.
	stream next.
	s := Integer 
		readFrom: stream
		base: 16.
	^ Array 
		with: r
		with: s
]

{ #category : #public }
DigitalSignatureAlgorithm >> verifySignature: aSignature ofMessageHash: hash publicKey: publicKey [
	"Answer true if the given signature is the authentic signature of the given message hash. That is, if the signature must have been computed using the private key set corresponding to the given public key. The public key is an array of four large integers: (p, q, g, y)."

	| p q g y r s w u1 u2 v0 v |
	p := publicKey first.
	q := publicKey second.
	g := publicKey third.
	y := publicKey fourth.
	r := aSignature first.
	s := aSignature last.
	((r > 0) and: [r < q]) ifFalse: [^ false].  "reject"
	((s > 0) and: [s < q]) ifFalse: [^ false].  "reject"

	w := s reciprocalModulo: q.
	u1 := (hash * w) \\ q.
	u2 := (r * w) \\ q.
	v0 := (g raisedTo: u1 modulo: p) * (y raisedTo: u2 modulo: p).
	v := ( v0 \\ p) \\ q.
	^ v = r

]
