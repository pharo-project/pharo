"
I am SHA256, a HashFunction.

I implement the SHA-2 cryptographic hash function with digest size 256.

See also

http://en.wikipedia.org/wiki/Sha256

http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf

Usage

SHA256 hashMessage: 'foo'.

From the older class comments:

Copied from CFSHA256 (Cloudfront)

Please direct questions or comments about this implementation to Ron Teitelbaum: Ron@USMedRec.com

This code was extensively coppied from SHA1 by Luciano Notarfrancesco lnotarfrancesco@yahoo.com
"
Class {
	#name : #SHA256,
	#superclass : #HashFunction,
	#instVars : [
		'totalA',
		'totalB',
		'totalC',
		'totalD',
		'totalE',
		'totalF',
		'totalG',
		'totalH',
		'totals'
	],
	#category : 'System-Hashing-SHA256'
}

{ #category : #accessing }
SHA256 class >> blockSize [
	^ 64
]

{ #category : #constants }
SHA256 class >> h0 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r6A09E667
]

{ #category : #constants }
SHA256 class >> h1 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16rBB67AE85
]

{ #category : #constants }
SHA256 class >> h2 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r3C6EF372
]

{ #category : #constants }
SHA256 class >> h3 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16rA54FF53A
]

{ #category : #constants }
SHA256 class >> h4 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r510E527F
]

{ #category : #constants }
SHA256 class >> h5 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r9B05688C
]

{ #category : #constants }
SHA256 class >> h6 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r1F83D9AB
]

{ #category : #constants }
SHA256 class >> h7 [
	"2 raisedTo: 32 times the square root of the first 8 primes 2..19"
	"#(2 3 5 7 11 13 17 19) collect: [:x | ((x sqrt) * (2.0 raisedTo: 32)) truncated hex] // 32 bits"
	^16r5BE0CD19
]

{ #category : #accessing }
SHA256 class >> hashSize [
	^ 32
]

{ #category : #constants }
SHA256 class >> roundConstants [
	"2 raisedTo: 32 times the cube root of the first 64 primes 2..311"
	"(Integer primesUpTo: 311) collect: [:x | ((x raisedTo: 1/3) * (2.0 raisedTo: 32)) truncated hex] \\ 32 bits" 
	^#(16r428A2F98 16r71374491 16rB5C0FBCF 16rE9B5DBA5 16r3956C25B 16r59F111F1 16r923F82A4 16rAB1C5ED5 16rD807AA98 16r12835B01 16r243185BE 16r550C7DC3 16r72BE5D74 16r80DEB1FE 16r9BDC06A7 16rC19BF174 16rE49B69C1 16rEFBE4786 16r0FC19DC6 16r240CA1CC 16r2DE92C6F 16r4A7484AA 16r5CB0A9DC 16r76F988DA 16r983E5152 16rA831C66D 16rB00327C8 16rBF597FC7 16rC6E00BF3 16rD5A79147 16r06CA6351 16r14292967 16r27B70A85 16r2E1B2138 16r4D2C6DFC 16r53380D13 16r650A7354 16r766A0ABB 16r81C2C92E 16r92722C85 16rA2BFE8A1 16rA81A664B 16rC24B8B70 16rC76C51A3 16rD192E819 16rD6990624 16rF40E3585 16r106AA070 16r19A4C116 16r1E376C08 16r2748774C 16r34B0BCB5 16r391C0CB3 16r4ED8AA4A 16r5B9CCA4F 16r682E6FF3 16r748F82EE 16r78A5636F 16r84C87814 16r8CC70208 16r90BEFFFA 16rA4506CEB 16rBEF9A3F7 16rC67178F2)
	
]

{ #category : #private }
SHA256 >> expandedBlock: aByteArray [
	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result." 
	| out src |
	out := Array new: 64.
	src := 1.
	1 to: 16 do: [:i |
		out at: i put: (ThirtyTwoBitRegister new loadFrom: aByteArray at: src).
		src := src + 4].

	17 to: 64 do: [:i | | s0 s1 wi15 wi2 wi |
		wi15 := (out at: i - 15) copy.
		wi2 := (out at: i-2) copy.
		s0 := wi15 copy.
		s0 leftRotateBy: -7;
			bitXor: (wi15 copy leftRotateBy: -18);
			bitXor: (wi15 copy bitShift: -3).
		s1 := wi2 copy.
		s1 leftRotateBy: -17;
			bitXor: (wi2 copy leftRotateBy: -19);
			bitXor: (wi2 copy bitShift: -10).
		wi := (out at: i-16) copy += s0 += (out at: i-7) copy += s1.
		out at: i put: wi].
	^ out

]

{ #category : #accessing }
SHA256 >> finalHash [
	"Concatenate the final totals to build the 256-bit integer result."
	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."

		^ (totalA asInteger bitShift: 224) +
		  (totalB asInteger bitShift: 192) +
		  (totalC asInteger bitShift: 160) +	
		  (totalD asInteger bitShift: 128) +
		  (totalE asInteger bitShift:  96) +
		  (totalF asInteger bitShift:  64) +
		  (totalG asInteger bitShift:  32) +
		  (totalH asInteger).
]

{ #category : #accessing }
SHA256 >> hashStream: aPositionableStream [
	"Hash the contents of the given stream from the current position to the end using SHA-256, the 256-bit variant of SHA-2. The SHA-2 family of algorithms are defined in FIPS PUB 180-4."
	"SHA256 new hashStream: (ReadStream on: 'foo')"

	| startPosition buf bitLength |
	self initializeTotals.

	aPositionableStream atEnd ifTrue: [ self processFinalBuffer: #() bitLength: 0 ].

	startPosition := aPositionableStream position.
	[ aPositionableStream atEnd ] whileFalse: [
		buf := aPositionableStream next: 64 into: (ByteArray new: 64).
		(aPositionableStream atEnd not and: [ buf size = 64 ])
			ifTrue: [ self processBuffer: buf ]
			ifFalse: [
				bitLength := (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength ] ].

	^self finalHash asByteArrayOfSize: 32
]

{ #category : #private }
SHA256 >> initializeTotals [
	"Initialize totalA through totalE to their seed values."

	"total registers for use when primitives are absent"
	totalA := ThirtyTwoBitRegister new load: self class h0.
	totalB := ThirtyTwoBitRegister new load: self class h1.
	totalC := ThirtyTwoBitRegister new load: self class h2.
	totalD := ThirtyTwoBitRegister new load: self class h3.
	totalE := ThirtyTwoBitRegister new load: self class h4.
	totalF := ThirtyTwoBitRegister new load: self class h5.
	totalG := ThirtyTwoBitRegister new load: self class h6.
	totalH := ThirtyTwoBitRegister new load: self class h7.
	self initializeTotalsArray.

]

{ #category : #private }
SHA256 >> initializeTotalsArray [
	"Initialize the totals array from the registers for use with the primitives."

	totals := WordArray new: 8.
	totals at: 1 put: totalA asInteger.
	totals at: 2 put: totalB asInteger.
	totals at: 3 put: totalC asInteger.
	totals at: 4 put: totalD asInteger.
	totals at: 5 put: totalE asInteger.
	totals at: 6 put: totalF asInteger.
	totals at: 7 put: totalG asInteger.
	totals at: 8 put: totalH asInteger.
]

{ #category : #private }
SHA256 >> processBuffer: aByteArray [
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."

	| a b c d e f g h w |
	totals := nil.

	"initialize registers a through e from the current totals" 
	a := totalA copy.
	b := totalB copy.
	c := totalC copy.
	d := totalD copy.
	e := totalE copy.
	f := totalF copy.
	g := totalG copy.
	h := totalH copy.

	"expand and process the buffer"
	w := self expandedBlock: aByteArray.
	1 to: 64 do: [:i | | s0 maj t0 s1 ch t1 |
		s0 := (a copy leftRotateBy: -2).
		s0 bitXor: (a copy leftRotateBy: -13);
			bitXor: (a copy leftRotateBy: -22).
		maj := (a copy bitAnd: b).
		maj bitOr: (b copy bitAnd: c);
			bitOr: (c copy bitAnd: a).
		t0 := s0 copy += maj.
		s1 := (e copy leftRotateBy: -6).
		s1 bitXor: (e copy leftRotateBy: -11);
			bitXor: (e copy leftRotateBy: -25).
		ch := (e copy bitAnd: f).
		ch bitOr: ((e copy bitInvert) bitAnd: g).
		t1 := h copy += s1 += ch += (ThirtyTwoBitRegister new load: (self class roundConstants at: i)) += (w at: i) copy.
		h := g.
		g := f.
		f := e.
		e := d copy += t1.
		d := c.
		c := b.
		b := a.
		a := t0 += t1.
	"Transcript cr; show: i asString, String tab, a asString, String tab, b asString, String tab, c asString, String tab, d asString, String tab, e asString, String tab, f asString, String tab, g asString, String tab, h asString, String tab."
	].

	"add a through e into total accumulators"
	totalA += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e.
	totalF += f.
	totalG += g.
	totalH += h.

]

{ #category : #private }
SHA256 >> processFinalBuffer: buffer bitLength: bitLength [
	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."

	| out |
	out := ByteArray new: 64.
	out replaceFrom: 1 to: buffer size with: buffer startingAt: 1.
	buffer size < 56 ifTrue: [  "padding and length fit in last data block"
		out at: buffer size + 1 put: 128.  "trailing one bit"
		self storeLength: bitLength in: out.  "end with length"
		self processBuffer: out.
		^ self].

	"process the final data block"
	buffer size < 64 ifTrue: [
		out at: buffer size + 1 put: 128].  "trailing one bit"
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.  "filled with zeros"
	buffer size = 64 ifTrue: [
		"add trailing one bit that didn't fit in final data block"
		out at: 1 put: 128].
	self storeLength: bitLength in: out.
	self processBuffer: out.

]

{ #category : #private }
SHA256 >> storeLength: bitLength in: aByteArray [
	"Fill in the final 8 bytes of the given ByteArray with a 64-bit big-endian representation of the original message length in bits."

	| n i |
	n := bitLength.
	i := aByteArray size.
	[n > 0] whileTrue: [
		aByteArray at: i put: (n bitAnd: 16rFF).
		n := n bitShift: -8.
		i := i - 1].

]
