"
My instances provide contiguous storage of bits, primarily to hold the graphical data of Forms. Forms and their subclasses provide the additional structural information as to how the bits should be interpreted in two dimensions.
"
Class {
	#name : #Bitmap,
	#superclass : #ArrayedCollection,
	#type : #words,
	#category : #'Graphics-Primitives'
}

{ #category : #'instance creation' }
Bitmap class >> decodeIntFrom: s [ 
	"Decode an integer in stream s as follows...
		0-223	0-223
		224-254	(0-30)*256 + next byte (0-7935)
		255		next 4 bytes	"
	| int |
	int := s next.
	int <= 223 ifTrue: [ ^ int ].
	int <= 254 ifTrue: [ ^ (int - 224) * 256 + s next ].
	int := s next.
	1 
		to: 3
		do: [ :j | int := (int bitShift: 8) + s next ].
	^ int
]

{ #category : #'instance creation' }
Bitmap class >> decompressFromByteArray: byteArray [ 
	| s bitmap size |
	s := byteArray readStream.
	size := self decodeIntFrom: s.
	bitmap := self new: size.
	bitmap 
		decompress: bitmap
		fromByteArray: byteArray
		at: s position + 1.
	^ bitmap
]

{ #category : #'class initialization' }
Bitmap class >> initialize [
	Smalltalk specialObjectsArray 
		at: 5
		put: self
]

{ #category : #'instance creation' }
Bitmap class >> newFromStream: s [ 
	| len |
	s next = 128 ifTrue: 
		[ "New compressed format"
		len := self decodeIntFrom: s.
		^ Bitmap decompressFromByteArray: (s nextInto: (ByteArray new: len)) ].
	s skip: -1.
	len := s nextInt32.
	len <= 0 
		ifTrue: 
			[ "Old compressed format"
			^ (self new: len negated) readCompressedFrom: s ]
		ifFalse: 
			[ "Old raw data format"
			^ s nextWordsInto: (self new: len) ]
]

{ #category : #utilities }
Bitmap class >> swapBytesIn: aNonPointerThing from: start to: stop [ 
	"Perform a bigEndian/littleEndian byte reversal of my words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	"The implementation is a hack, but fast for large ranges"
	| hack blt |
	hack := Form new hackBits: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.	"XOR"
	blt
		sourceY: start - 1;
		destY: start - 1;
		height: stop - start + 1;
		width: 1.
	blt
		sourceX: 0;
		destX: 3;
		copyBits.	"Exchange bytes 0 and 3"
	blt
		sourceX: 3;
		destX: 0;
		copyBits.
	blt
		sourceX: 0;
		destX: 3;
		copyBits.
	blt
		sourceX: 1;
		destX: 2;
		copyBits.	"Exchange bytes 1 and 2"
	blt
		sourceX: 2;
		destX: 1;
		copyBits.
	blt
		sourceX: 1;
		destX: 2;
		copyBits
]

{ #category : #conversion }
Bitmap >> asByteArray [
	"Faster way to make a byte array from me.
	copyFromByteArray: makes equal Bitmap."
	| f bytes hack |
	f := Form 
		extent: 4 @ self size
		depth: 8
		bits: self.
	bytes := ByteArray new: self size * 4.
	hack := Form new hackBits: bytes.
	EndianDetector isLittleEndian ifTrue: [ hack swapEndianness ].
	hack 
		copyBits: f boundingBox
		from: f
		at: 0 @ 0
		clippingBox: hack boundingBox
		rule: Form over
		fillColor: nil
		map: nil.

	"f displayOn: hack."
	^ bytes
]

{ #category : #accessing }
Bitmap >> atAllPut: value [
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."
	<primitive: 145>
	super atAllPut: value.
]

{ #category : #accessing }
Bitmap >> bitPatternForDepth: depth [
	"The raw call on BitBlt needs a Bitmap to represent this color.  I already am Bitmap like. I am already adjusted for a specific depth.  Interpret me as an array of (32/depth) Color pixelValues. BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary."

	^ self
]

{ #category : #accessing }
Bitmap >> byteAt: byteAddress [ 
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form>>#pixelAt:"
	| lowBits |
	lowBits := byteAddress - 1 bitAnd: 3.
	^ ((self at: (byteAddress - 1 - lowBits) // 4 + 1) bitShift: (lowBits - 3) * 8) bitAnd: 255
]

{ #category : #accessing }
Bitmap >> byteAt: byteAddress put: byte [ 
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form>>#pixelAt:put:"
	| longWord shift lowBits longAddr |
	(byte < 0 or: [ byte > 255 ]) ifTrue: [ ^ self errorImproperStore ].
	lowBits := byteAddress - 1 bitAnd: 3.
	longWord := self at: (longAddr := (byteAddress - 1 - lowBits) // 4 + 1).
	shift := (3 - lowBits) * 8.
	longWord := longWord - (longWord bitAnd: (255 bitShift: shift)) + (byte bitShift: shift).
	self 
		at: longAddr
		put: longWord.
	^ byte
]

{ #category : #accessing }
Bitmap >> byteSize [
	^self size * 4
]

{ #category : #filing }
Bitmap >> compress: bm toByteArray: ba [
	"Store a run-coded compression of the receiver into the byteArray ba,
	and return the last index stored into. ba is assumed to be large enough.
	The encoding is as follows...
		S {N D}*.
		S is the size of the original bitmap, followed by run-coded pairs.
		N is a run-length * 4 + data code.
		D, the data, depends on the data code...
			0	skip N words, D is absent
			1	N words with all 4 bytes = D (1 byte)
			2	N words all = D (4 bytes)
			3	N words follow in D (4N bytes)
		S and N are encoded as follows...
			0-223	0-223
			224-254	(0-30)*256 + next byte (0-7935)
			255		next 4 bytes"
	| size k word j lowByte eqBytes i |
	<primitive: 'primitiveCompressToByteArray' module: 'MiscPrimitivePlugin'>
	
	<var: #bm declareC: 'int *bm'>
	<var: #ba declareC: 'unsigned char *ba'>
	
	size := bm size.
	i := self 
		encodeInt: size
		in: ba
		at: 1.
	k := 1.
	[ k <= size ] whileTrue: 
		[ word := bm at: k.
		lowByte := word bitAnd: 255.
		eqBytes := (word >> 8 bitAnd: 255) = lowByte and: 
			[ (word >> 16 bitAnd: 255) = lowByte and: [ (word >> 24 bitAnd: 255) = lowByte ] ].
		j := k.
		[ j < size and: [ word = (bm at: j + 1) ]	"scan for = words..." ] whileTrue: [ j := j + 1 ].
		j > k 
			ifTrue: 
				[ "We have two or more = words, ending at j"
				eqBytes 
					ifTrue: 
						[ "Actually words of = bytes"
						i := self 
							encodeInt: (j - k + 1) * 4 + 1
							in: ba
							at: i.
						ba 
							at: i
							put: lowByte.
						i := i + 1 ]
					ifFalse: 
						[ i := self 
							encodeInt: (j - k + 1) * 4 + 2
							in: ba
							at: i.
						i := self 
							encodeBytesOf: word
							in: ba
							at: i ].
				k := j + 1 ]
			ifFalse: 
				[ "Check for word of 4 = bytes"
				eqBytes 
					ifTrue: 
						[ "Note 1 word of 4 = bytes"
						i := self 
							encodeInt: 1 * 4 + 1
							in: ba
							at: i.
						ba 
							at: i
							put: lowByte.
						i := i + 1.
						k := k + 1 ]
					ifFalse: 
						[ "Finally, check for junk"
						[ j < size and: [ (bm at: j) ~= (bm at: j + 1) ]	"scan for ~= words..." ] whileTrue: [ j := j + 1 ].
						j = size ifTrue: [ j := j + 1 ].
						"We have one or more unmatching words, ending at j-1"
						i := self 
							encodeInt: (j - k) * 4 + 3
							in: ba
							at: i.
						k 
							to: j - 1
							do: 
								[ :m | 
								i := self 
									encodeBytesOf: (bm at: m)
									in: ba
									at: i ].
						k := j ] ] ].
	^ i - 1	"number of bytes actually stored"
]

{ #category : #filing }
Bitmap >> compressToByteArray [
	"Return a run-coded compression of this bitmap into a byteArray"
	"Without skip codes, it is unlikely that the compressed bitmap will be any larger than was the original.  The run-code cases are...
	N >= 1 words of equal bytes:  4N bytes -> 2 bytes (at worst 4 -> 2)
	N > 1 equal words:  4N bytes -> 5 bytes (at worst 8 -> 5)
	N > 1 unequal words:  4N bytes -> 4N + M, where M is the number of bytes required to encode the run length.

The worst that can happen is that the method begins with unequal words, and than has interspersed occurrences of a word with equal bytes.  Thus we require a run-length at the beginning, and after every interspersed word of equal bytes.  However, each of these saves 2 bytes, so it must be followed by a run of 1984 (7936//4) or more (for which M jumps from 2 to 5) to add any extra overhead.  Therefore the worst case is a series of runs of 1984 or more, with single interspersed words of equal bytes.  At each break we save 2 bytes, but add 5.  Thus the overhead would be no more than 5 (encoded size) + 2 (first run len) + (S//1984*3)."
	"NOTE: This code is copied in Form hibernate for reasons given there."
	| byteArray lastByte |
	byteArray := ByteArray new: self size * 4 + 7 + (self size // 1984 * 3).
	lastByte := self 
		compress: self
		toByteArray: byteArray.
	^ byteArray 
		copyFrom: 1
		to: lastByte
]

{ #category : #accessing }
Bitmap >> copyFromByteArray: byteArray [ 
	"This method should work with either byte orderings"

	| myHack byteHack |
	myHack := Form new hackBits: self.
	byteHack := Form new hackBits: byteArray.
	EndianDetector isLittleEndian ifTrue: [byteHack swapEndianness].
	byteHack displayOn: myHack
]

{ #category : #filing }
Bitmap >> decompress: bm fromByteArray: ba at: index [ 
	"Decompress the body of a byteArray encoded by compressToByteArray (qv)...
	The format is simply a sequence of run-coded pairs, {N D}*.
		N is a run-length * 4 + data code.
		D, the data, depends on the data code...
			0	skip N words, D is absent
				(could be used to skip from one raster line to the next)
			1	N words with all 4 bytes = D (1 byte)
			2	N words all = D (4 bytes)
			3	N words follow in D (4N bytes)
		S and N are encoded as follows (see decodeIntFrom:)...
			0-223	0-223
			224-254	(0-30)*256 + next byte (0-7935)
			255		next 4 bytes"
	"NOTE:  If fed with garbage, this routine could read past the end of ba, but it should fail before writing past the ned of bm."
	| i code n anInt data end k pastEnd |
	<primitive: 'primitiveDecompressFromByteArray' module: 'MiscPrimitivePlugin'>
	
	<var: #bm declareC: 'int *bm'>
	<var: #ba declareC: 'unsigned char *ba'>
	
	i := index.	"byteArray read index"
	end := ba size.
	k := 1.	"bitmap write index"
	pastEnd := bm size + 1.
	[ i <= end ] whileTrue: 
		[ "Decode next run start N"
		anInt := ba at: i.
		i := i + 1.
		anInt <= 223 ifFalse: 
			[ anInt <= 254 
				ifTrue: 
					[ anInt := (anInt - 224) * 256 + (ba at: i).
					i := i + 1 ]
				ifFalse: 
					[ anInt := 0.
					1 
						to: 4
						do: 
							[ :j | 
							anInt := (anInt bitShift: 8) + (ba at: i).
							i := i + 1 ] ] ].
		n := anInt >> 2.
		k + n > pastEnd ifTrue: [ ^ self primitiveFail ].
		code := anInt bitAnd: 3.
		code = 0 ifTrue: 
			[ "skip"
			 ].
		code = 1 ifTrue: 
			[ "n consecutive words of 4 bytes = the following byte"
			data := ba at: i.
			i := i + 1.
			data := data bitOr: (data bitShift: 8).
			data := data bitOr: (data bitShift: 16).
			1 
				to: n
				do: 
					[ :j | 
					bm 
						at: k
						put: data.
					k := k + 1 ] ].
		code = 2 ifTrue: 
			[ "n consecutive words = 4 following bytes"
			data := 0.
			1 
				to: 4
				do: 
					[ :j | 
					data := (data bitShift: 8) bitOr: (ba at: i).
					i := i + 1 ].
			1 
				to: n
				do: 
					[ :j | 
					bm 
						at: k
						put: data.
					k := k + 1 ] ].
		code = 3 ifTrue: 
			[ "n consecutive words from the data..."
			1 
				to: n
				do: 
					[ :m | 
					data := 0.
					1 
						to: 4
						do: 
							[ :j | 
							data := (data bitShift: 8) bitOr: (ba at: i).
							i := i + 1 ].
					bm 
						at: k
						put: data.
					k := k + 1 ] ] ]
]

{ #category : #accessing }
Bitmap >> defaultElement [
	"Return the default element of the receiver"
	^0
]

{ #category : #filing }
Bitmap >> encodeBytesOf: anInt in: ba at: i [
	"Copy the integer anInt into byteArray ba at index i, and return the next index"

	<inline: true>
	<var: #ba declareC: 'unsigned char *ba'>
	
	0 to: 3 do:
		[:j | ba at: i+j put: (anInt >> (3-j*8) bitAnd: 16rFF)].
	^ i+4
]

{ #category : #filing }
Bitmap >> encodeInt: int [ 
	"Encode the integer int as per encodeInt:in:at:, and return it as a ByteArray"
	| byteArray next |
	byteArray := ByteArray new: 5.
	next := self 
		encodeInt: int
		in: byteArray
		at: 1.
	^ byteArray 
		copyFrom: 1
		to: next - 1
]

{ #category : #filing }
Bitmap >> encodeInt: anInt in: ba at: i [
	"Encode the integer anInt in byteArray ba at index i, and return the next index.
	The encoding is as follows...
		0-223	0-223
		224-254	(0-30)*256 + next byte (0-7935)
		255		next 4 bytes"		

	<inline: true>
	<var: #ba declareC: 'unsigned char *ba'>
	
	anInt <= 223 ifTrue: [ba at: i put: anInt. ^ i+1].
	anInt <= 7935 ifTrue: [ba at: i put: anInt//256+224. ba at: i+1 put: anInt\\256.  ^ i+2].
	ba at: i put: 255.
	^ self encodeBytesOf: anInt in: ba at: i+1
]

{ #category : #initialize }
Bitmap >> fromByteStream: aStream [ 
	"Initialize the array of bits by reading integers from the argument, 
	aStream."
	aStream nextWordsInto: self
]

{ #category : #accessing }
Bitmap >> integerAt: index [ 
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 1073741823 ifTrue: [ ^ word ].	"Avoid LargeInteger computations"
	^ word >= 2147483648 
		ifTrue: 
			[ "Negative?!"
			"word - 16r100000000"
			(word bitInvert32 + 1) negated ]
		ifFalse: [ word ]
]

{ #category : #accessing }
Bitmap >> integerAt: index put: anInteger [ 
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0 
		ifTrue: 
			[ "word := 16r100000000 + anInteger"
			word := (anInteger + 1) negated bitInvert32 ]
		ifFalse: [ word := anInteger ].
	self 
		basicAt: index
		put: word.
	^ anInteger
]

{ #category : #testing }
Bitmap >> isColormap [
	"Bitmaps were used as color maps for BitBlt.
	This method allows to recognize real color maps."
	^false
]

{ #category : #accessing }
Bitmap >> pixelValueForDepth: depth [
	"Self is being used to represent a single color.  Answer bits that appear in ONE pixel of this color in a Bitmap of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32.  Returns an integer.  First pixel only.  "

	^ (self at: 1) bitAnd: (1 bitShift: depth) - 1
]

{ #category : #accessing }
Bitmap >> primFill: aPositiveInteger [
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.
]

{ #category : #printing }
Bitmap >> printOn: aStream [
	self printNameOn: aStream.
	aStream nextPutAll: ' of length '; print: self size
]

{ #category : #filing }
Bitmap >> readCompressedFrom: strm [ 
	"Decompress an old-style run-coded stream into this bitmap:
		[0 means end of runs]
		[n = 1..127] [(n+3) copies of next byte]
		[n = 128..191] [(n-127) next bytes as is]
		[n = 192..255] [(n-190) copies of next 4 bytes]"
	| n byte out outBuff bytes |
	out := (outBuff := ByteArray new: self size * 4) writeStream.
	[ (n := strm next) > 0 ] whileTrue: 
		[ (n 
			between: 1
			and: 127) ifTrue: 
			[ byte := strm next.
			1 
				to: n + 3
				do: [ :i | out nextPut: byte ] ].
		(n 
			between: 128
			and: 191) ifTrue: 
			[ 1 
				to: n - 127
				do: [ :i | out nextPut: strm next ] ].
		(n 
			between: 192
			and: 255) ifTrue: 
			[ bytes := (1 to: 4) collect: [ :i | strm next ].
			1 
				to: n - 190
				do: [ :i | bytes do: [ :b | out nextPut: b ] ] ] ].
	out position = outBuff size ifFalse: [ self error: 'Decompression size error' ].
	"Copy the final byteArray into self"
	self copyFromByteArray: outBuff
]

{ #category : #accessing }
Bitmap >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : #conversion }
Bitmap >> restoreEndianness [
	"nothing to do here?"
]

{ #category : #filing }
Bitmap >> storeBits: startBit to: stopBit on: aStream [ 
	"Store my bits as a hex string, breaking the lines every 100 bytes or 
	so to comply with the maximum line length limits of Postscript (255 
	bytes). "
	| lineWidth |
	lineWidth := 0.
	self
		do: [:word | 
			startBit
				to: stopBit
				by: -4
				do: [:shift | 
					aStream nextPut: (word >> shift bitAnd: 15) asHexDigit.
					lineWidth := lineWidth + 1].
			(lineWidth > 100)
				ifTrue: [aStream cr.
					lineWidth := 0]].
	lineWidth > 0 ifTrue: [ aStream cr ].
]

{ #category : #filing }
Bitmap >> writeOn: aStream [ 
	"Store the array of bits onto the argument, aStream. A leading byte of 16r80 identifies this as compressed by compressToByteArray (qv)."
	| b |
	aStream nextPut: 128.
	b := self compressToByteArray.
	aStream
		nextPutAll: (self encodeInt: b size);
		nextPutAll: b
]

{ #category : #filing }
Bitmap >> writeUncompressedOn: aStream [ 
	"Store the array of bits onto the argument, aStream.
	(leading byte ~= 16r80) identifies this as raw bits (uncompressed)."

	aStream nextInt32Put: self size.
	aStream nextPutAll: self

]
