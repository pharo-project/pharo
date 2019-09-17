"
This class implements the Secure Hash Algorithm (SHA) described in the U.S. government's Secure Hash Standard (SHS). This standard is described in FIPS PUB 180-1, ""SECURE HASH STANDARD"", April 17, 1995.

The Secure Hash Algorithm is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Schneier, Wiley, 1996.

See the comment in class DigitalSignatureAlgorithm for details on its use.

Implementation notes:
The secure hash standard was created with 32-bit hardware in mind. All arithmetic in the hash computation must be done modulo 2^32. This implementation uses ThirtyTwoBitRegister objects to simulate hardware registers; this implementation is about six times faster than using LargePositiveIntegers (measured on a Macintosh G3 Powerbook). Implementing a primitive to process each 64-byte buffer would probably speed up the computation by a factor of 20 or more.

"
Class {
	#name : #SHA1,
	#superclass : #HashFunction,
	#instVars : [
		'totalA',
		'totalB',
		'totalC',
		'totalD',
		'totalE',
		'totals'
	],
	#classVars : [
		'K1',
		'K2',
		'K3',
		'K4'
	],
	#category : #'System-Hashing-SHA1'
}

{ #category : #accessing }
SHA1 class >> blockSize [
	^ 64
]

{ #category : #accessing }
SHA1 class >> hashSize [
	^ 20
]

{ #category : #'class initialization' }
SHA1 class >> initialize [
	"SecureHashAlgorithm initialize"
	"For the curious, here's where these constants come from:
	  #(2 3 5 10) collect: [:x | ((x sqrt / 4.0) * (2.0 raisedTo: 32)) truncated hex]"
	K1 := ThirtyTwoBitRegister new load: 1518500249.
	K2 := ThirtyTwoBitRegister new load: 1859775393.
	K3 := ThirtyTwoBitRegister new load: 2400959708.
	K4 := ThirtyTwoBitRegister new load: 3395469782
]

{ #category : #private }
SHA1 >> constantForStep: i [
	"Answer the constant for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."

	i <= 20 ifTrue: [^ K1].
	i <= 40 ifTrue: [^ K2].
	i <= 60 ifTrue: [^ K3].
	^ K4

]

{ #category : #private }
SHA1 >> expandedBlock: aByteArray [ 
	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result."
	| out src v |
	out := Array new: 80.
	src := 1.
	1 
		to: 16
		do: 
			[ :i | 
			out 
				at: i
				put: (ThirtyTwoBitRegister new 
						loadFrom: aByteArray
						at: src).
			src := src + 4 ].
	17 
		to: 80
		do: 
			[ :i | 
			v := (out at: i - 3) copy.
			v
				bitXor: (out at: i - 8);
				bitXor: (out at: i - 14);
				bitXor: (out at: i - 16);
				leftRotateBy: 1.
			out 
				at: i
				put: v ].
	^ out
]

{ #category : #private }
SHA1 >> finalHash [
	"Concatenate the final totals to build the 160-bit integer result."
	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."
	| r |
	totals ifNil: 
		[ "compute final hash when not using primitives"
		^ (totalA asInteger bitShift: 128) + (totalB asInteger bitShift: 96) + (totalC asInteger bitShift: 64) + (totalD asInteger bitShift: 32) + totalE asInteger ].

	"compute final hash when using primitives"
	r := 0.
	1 
		to: 5
		do: [ :i | r := r bitOr: ((totals at: i) bitShift: 32 * (5 - i)) ].
	^ r
]

{ #category : #private }
SHA1 >> hashFunction: i of: x with: y with: z [
	"Compute the hash function for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."
	"Details: There are four functions, one for each 20 iterations. The second and fourth are the same."

	i <= 20 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)].
	i <= 40 ifTrue: [^ x copy bitXor: y; bitXor: z].
	i <= 60 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitAnd: z); bitOr: (y copy bitAnd: z)].
	^ x copy bitXor: y; bitXor: z

]

{ #category : #accessing }
SHA1 >> hashInteger: aPositiveInteger [ 
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in key generation."
	| buffer dstIndex |
	self initializeTotals.

	"pad integer with zeros"
	aPositiveInteger highBit <= 512 ifFalse: [ self error: 'integer cannot exceed 512 bits' ].
	buffer := ByteArray new: 64.
	dstIndex := 0.
	aPositiveInteger bytesCount 
		to: 1
		by: -1
		do: 
			[ :i | 
			buffer 
				at: (dstIndex := dstIndex + 1)
				put: (aPositiveInteger byteAt: i) ].

	"process that one block"
	self processBuffer: buffer.
	^ self finalHash
]

{ #category : #accessing }
SHA1 >> hashInteger: aPositiveInteger seed: seedInteger [ 
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in the production of random numbers"
	"Initialize totalA through totalE to their seed values."
	| buffer dstIndex |
	totalA := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -128) bitAnd: 4294967295).
	totalB := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -96) bitAnd: 4294967295).
	totalC := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -64) bitAnd: 4294967295).
	totalD := ThirtyTwoBitRegister new load: ((seedInteger bitShift: -32) bitAnd: 4294967295).
	totalE := ThirtyTwoBitRegister new load: (seedInteger bitAnd: 4294967295).
	self initializeTotalsArray.

	"pad integer with zeros"
	buffer := ByteArray new: 64.
	dstIndex := 0.
	aPositiveInteger bytesCount 
		to: 1
		by: -1
		do: 
			[ :i | 
			buffer 
				at: (dstIndex := dstIndex + 1)
				put: (aPositiveInteger byteAt: i) ].

	"process that one block"
	self processBuffer: buffer.
	^ self finalHash
]

{ #category : #public }
SHA1 >> hashStream: aPositionableStream [
	"Hash the contents of the given stream from the current position to the end using the Secure Hash Algorithm. The SHA algorithm is defined in FIPS PUB 180-1. It is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Schneier, Wiley, 1996."
	"http://en.wikipedia.org/wiki/Sha1#Example_hashes" 
	"(SHA1 new hashStream: 'The quick brown fox jumps over the lazy dog' readStream) hex."
	
	| startPosition buf bitLength |
	self initializeTotals.
  	
	aPositionableStream atEnd ifTrue: [self processFinalBuffer: #() bitLength: 0].

	startPosition := aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: 
		[ buf := aPositionableStream next: 64.
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [ bitLength := (aPositionableStream position - startPosition) * 8.
					self processFinalBuffer: buf bitLength: bitLength]].
	^ self finalHash asByteArrayOfSize: 20
]

{ #category : #private }
SHA1 >> initializeTotals [
	"Initialize totalA through totalE to their seed values."
	"total registers for use when primitives are absent"
	totalA := ThirtyTwoBitRegister new load: 1732584193.
	totalB := ThirtyTwoBitRegister new load: 4023233417.
	totalC := ThirtyTwoBitRegister new load: 2562383102.
	totalD := ThirtyTwoBitRegister new load: 271733878.
	totalE := ThirtyTwoBitRegister new load: 3285377520.
	self initializeTotalsArray
]

{ #category : #private }
SHA1 >> initializeTotalsArray [
	"Initialize the totals array from the registers for use with the primitives."
	totals := WordArray new: 5.
	totals 
		at: 1
		put: totalA asInteger.
	totals 
		at: 2
		put: totalB asInteger.
	totals 
		at: 3
		put: totalC asInteger.
	totals 
		at: 4
		put: totalD asInteger.
	totals 
		at: 5
		put: totalE asInteger
]

{ #category : #primitives }
SHA1 >> primExpandBlock: aByteArray into: wordBitmap [
	"Expand the given 64-byte buffer into the given Bitmap of length 80."

	<primitive: 'primitiveExpandBlock' module: 'DSAPrims'>
	^ self primitiveFailed

]

{ #category : #primitives }
SHA1 >> primHasSecureHashPrimitive [
	"Answer true if this platform has primitive support for the Secure Hash Algorithm."

	<primitive: 'primitiveHasSecureHashPrimitive' module: 'DSAPrims'>
	^ false

]

{ #category : #primitives }
SHA1 >> primHashBlock: blockBitmap using: workingTotalsBitmap [
	"Hash the given block (a Bitmap) of 80 32-bit words, using the given workingTotals."

	<primitive: 'primitiveHashBlock' module: 'DSAPrims'>
	^ self primitiveFailed

]

{ #category : #private }
SHA1 >> processBuffer: aByteArray [ 
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."
	| a b c d e w tmp |
	self primHasSecureHashPrimitive 
		ifTrue: [ ^ self processBufferUsingPrimitives: aByteArray ]
		ifFalse: [ totals := nil ].

	"initialize registers a through e from the current totals"
	a := totalA copy.
	b := totalB copy.
	c := totalC copy.
	d := totalD copy.
	e := totalE copy.

	"expand and process the buffer"
	w := self expandedBlock: aByteArray.
	1 
		to: 80
		do: 
			[ :i | 
			tmp := (a copy leftRotateBy: 5)
				+= (self 
						hashFunction: i
						of: b
						with: c
						with: d);
				+= e;
				+= (w at: i);
				+= (self constantForStep: i).
			e := d.
			d := c.
			c := b copy leftRotateBy: 30.
			b := a.
			a := tmp ].

	"add a through e into total accumulators"
	totalA += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e
]

{ #category : #private }
SHA1 >> processBufferUsingPrimitives: aByteArray [ 
	"Process given 64-byte buffer using the primitives, accumulating the results in totals."
	"expand and process the buffer"
	| w |
	w := WordArray new: 80.
	self 
		primExpandBlock: aByteArray
		into: w.
	self 
		primHashBlock: w
		using: totals
]

{ #category : #private }
SHA1 >> processFinalBuffer: buffer bitLength: bitLength [ 
	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."
	| out |
	out := ByteArray new: 64.
	out 
		replaceFrom: 1
		to: buffer size
		with: buffer
		startingAt: 1.
	buffer size < 56 ifTrue: 
		[ "padding and length fit in last data block"
		out 
			at: buffer size + 1
			put: 128.	"trailing one bit"
		self 
			storeLength: bitLength
			in: out.	"end with length"
		self processBuffer: out.
		^ self ].

	"process the final data block"
	buffer size < 64 ifTrue: 
		[ out 
			at: buffer size + 1
			put: 128 ].	"trailing one bit"
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.	"filled with zeros"
	buffer size = 64 ifTrue: 
		[ "add trailing one bit that didn't fit in final data block"
		out 
			at: 1
			put: 128 ].
	self 
		storeLength: bitLength
		in: out.
	self processBuffer: out
]

{ #category : #private }
SHA1 >> storeLength: bitLength in: aByteArray [ 
	"Fill in the final 8 bytes of the given ByteArray with a 64-bit big-endian representation of the original message length in bits."
	| n i |
	n := bitLength.
	i := aByteArray size.
	[ n > 0 ] whileTrue: 
		[ aByteArray 
			at: i
			put: (n bitAnd: 255).
		n := n bitShift: -8.
		i := i - 1 ]
]
