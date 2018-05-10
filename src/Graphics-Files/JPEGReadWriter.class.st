"
I am a subclass of ImageReadWriter that understands JFIF file streams, and can decode JPEG images.
This code is based upon the Independent Joint Photographic Experts Group (IJPEG) software, originally written in C by Tom Lane, Philip Gladstone, Luis Ortiz, Jim Boucher, Lee Crocker, Julian Minguillon, George Phillips, Davide Rossi, Ge' Weijers, and other members of the Independent JPEG Group.


"
Class {
	#name : #JPEGReadWriter,
	#superclass : #ImageReadWriter,
	#instVars : [
		'width',
		'height',
		'components',
		'currentComponents',
		'qTable',
		'hACTable',
		'hDCTable',
		'restartInterval',
		'restartsToGo',
		'mcuWidth',
		'mcuHeight',
		'mcusPerRow',
		'mcuRowsInScan',
		'mcuMembership',
		'mcuSampleBuffer',
		'mcuImageBuffer',
		'majorVersion',
		'minorVersion',
		'dataPrecision',
		'densityUnit',
		'xDensity',
		'yDensity',
		'ss',
		'se',
		'ah',
		'al',
		'sosSeen',
		'residuals',
		'ditherMask'
	],
	#classVars : [
		'ConstBits',
		'DCTK1',
		'DCTK2',
		'DCTK3',
		'DCTK4',
		'DCTSize',
		'DCTSize2',
		'DitherMasks',
		'FIXn0n298631336',
		'FIXn0n34414',
		'FIXn0n390180644',
		'FIXn0n541196100',
		'FIXn0n71414',
		'FIXn0n765366865',
		'FIXn0n899976223',
		'FIXn1n175875602',
		'FIXn1n40200',
		'FIXn1n501321110',
		'FIXn1n77200',
		'FIXn1n847759065',
		'FIXn1n961570560',
		'FIXn2n053119869',
		'FIXn2n562915447',
		'FIXn3n072711026',
		'FloatSampleOffset',
		'HuffmanTableSize',
		'JFIFMarkerParser',
		'JPEGNaturalOrder',
		'MaxSample',
		'Pass1Bits',
		'Pass1Div',
		'Pass2Div',
		'QTableScaleFactor',
		'QuantizationTableSize',
		'SampleOffset'
	],
	#category : #'Graphics-Files'
}

{ #category : #initialization }
JPEGReadWriter class >> initialize [
	"JPEGReadWriter initialize"
	"general constants"
	DCTSize := 8.
	MaxSample := (2 raisedToInteger: DCTSize) - 1.
	SampleOffset := MaxSample // 2.
	FloatSampleOffset := SampleOffset asFloat.
	DCTSize2 := DCTSize squared.
	QuantizationTableSize := 4.
	HuffmanTableSize := 4.

	"floating-point Inverse Discrete Cosine Transform (IDCT) constants"
	ConstBits := 13.
	Pass1Bits := 2.
	DCTK1 := 2 sqrt.
	DCTK2 := 1.847759065.
	DCTK3 := 1.0823922.
	DCTK4 := -2.61312593.
	Pass1Div := 1 bitShift: ConstBits - Pass1Bits.
	Pass2Div := 1 bitShift: ConstBits + Pass1Bits + 3.

	"fixed-point Inverse Discrete Cosine Transform (IDCT) constants"
	FIXn0n298631336 := 2446.
	FIXn0n390180644 := 3196.
	FIXn0n541196100 := 4433.
	FIXn0n765366865 := 6270.
	FIXn0n899976223 := 7373.
	FIXn1n175875602 := 9633.
	FIXn1n501321110 := 12299.
	FIXn1n847759065 := 15137.
	FIXn1n961570560 := 16069.
	FIXn2n053119869 := 16819.
	FIXn2n562915447 := 20995.
	FIXn3n072711026 := 25172.

	"fixed-point color conversion constants"
	FIXn0n34414 := 22554.
	FIXn0n71414 := 46802.
	FIXn1n40200 := 91881.
	FIXn1n77200 := 116130.

	"reordering table from JPEG zig-zag order"
	JPEGNaturalOrder := #(
		1
		2
		9
		17
		10
		3
		4
		11
		18
		25
		33
		26
		19
		12
		5
		6
		13
		20
		27
		34
		41
		49
		42
		35
		28
		21
		14
		7
		8
		15
		22
		29
		36
		43
		50
		57
		58
		51
		44
		37
		30
		23
		16
		24
		31
		38
		45
		52
		59
		60
		53
		46
		39
		32
		40
		47
		54
		61
		62
		55
		48
		56
		63
		64
	).

	"scale factors for the values in the Quantization Tables"
	QTableScaleFactor := (0 to: DCTSize - 1) collect: 
		[ :k | 
		k = 0 
			ifTrue: [ 1.0 ]
			ifFalse: [ (k * Float pi / 16) cos * 2 sqrt ] ].

	"dithering masks"
	(DitherMasks := Dictionary new)
		add: 0 -> 0;
		add: 1 -> 127;
		add: 2 -> 63;
		add: 4 -> 63;
		add: 8 -> 31;
		add: 16 -> 7;
		add: 32 -> 0.

	"dictionary of marker parsers"
	(JFIFMarkerParser := Dictionary new)
		add: 1 -> #parseNOP;
		add: 192 -> #parseStartOfFile;
		add: 196 -> #parseHuffmanTable;
		addAll: ((208 to: 215) collect: 
				[ :m | 
				Association 
					key: m
					value: #parseNOP ]);
		add: 216 -> #parseStartOfInput;
		add: 217 -> #parseEndOfInput;
		add: 218 -> #parseStartOfScan;
		add: 219 -> #parseQuantizationTable;
		add: 221 -> #parseDecoderRestartInterval;
		add: 224 -> #parseAPPn;
		add: 225 -> #parseAPPn
]

{ #category : #'image reading/writing' }
JPEGReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('jpg' 'jpeg')
]

{ #category : #'image reading/writing' }
JPEGReadWriter class >> understandsImageFormat: aStream [
	(PluginBasedJPEGReadWriter understandsImageFormat: aStream) ifTrue:[^false].
	aStream next = 16rFF ifFalse: [^ false].
	aStream next = 16rD8 ifFalse: [^ false].
	^true
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> colorConvertFloatYCbCrMCU [
	| ySampleStream crSampleStream cbSampleStream y cb cr red green blue bits |
	ySampleStream := currentComponents at: 1.
	cbSampleStream := currentComponents at: 2.
	crSampleStream := currentComponents at: 3.
	ySampleStream resetSampleStream.
	cbSampleStream resetSampleStream.
	crSampleStream resetSampleStream.
	bits := mcuImageBuffer bits.
	1 
		to: bits size
		do: 
			[ :i | 
			y := ySampleStream nextSample.
			cb := cbSampleStream nextSample - FloatSampleOffset.
			cr := crSampleStream nextSample - FloatSampleOffset.
			red := self sampleFloatRangeLimit: y + (1.402 * cr).
			green := self sampleFloatRangeLimit: y - (0.34414 * cb) - (0.71414 * cr).
			blue := self sampleFloatRangeLimit: y + (1.772 * cb).
			bits 
				at: i
				put: 4278190080 + (red << 16) + (green << 8) + blue ]
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> colorConvertGrayscaleMCU [
	| ySampleStream y bits |
	ySampleStream := currentComponents at: 1.
	ySampleStream resetSampleStream.
	bits := mcuImageBuffer bits.
	1 
		to: bits size
		do: 
			[ :i | 
			y := ySampleStream nextSample + (residuals at: 2).
			y > MaxSample ifTrue: [ y := MaxSample ].
			residuals 
				at: 2
				put: (y bitAnd: ditherMask).
			y := y bitAnd: MaxSample - ditherMask.
			y < 1 ifTrue: [ y := 1 ].
			bits 
				at: i
				put: 4278190080 + (y << 16) + (y << 8) + y ]
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> colorConvertIntYCbCrMCU [
	| ySampleStream crSampleStream cbSampleStream y cb cr red green blue bits |
	ySampleStream := currentComponents at: 1.
	cbSampleStream := currentComponents at: 2.
	crSampleStream := currentComponents at: 3.
	ySampleStream resetSampleStream.
	cbSampleStream resetSampleStream.
	crSampleStream resetSampleStream.
	bits := mcuImageBuffer bits.
	1 
		to: bits size
		do: 
			[ :i | 
			y := ySampleStream nextSample.
			cb := cbSampleStream nextSample - SampleOffset.
			cr := crSampleStream nextSample - SampleOffset.
			red := y + (FIXn1n40200 * cr // 65536) + (residuals at: 1).
			red > MaxSample 
				ifTrue: [ red := MaxSample ]
				ifFalse: [ red < 0 ifTrue: [ red := 0 ] ].
			residuals 
				at: 1
				put: (red bitAnd: ditherMask).
			red := red bitAnd: MaxSample - ditherMask.
			red < 1 ifTrue: [ red := 1 ].
			green := y - (FIXn0n34414 * cb // 65536) - (FIXn0n71414 * cr // 65536) + (residuals at: 2).
			green > MaxSample 
				ifTrue: [ green := MaxSample ]
				ifFalse: [ green < 0 ifTrue: [ green := 0 ] ].
			residuals 
				at: 2
				put: (green bitAnd: ditherMask).
			green := green bitAnd: MaxSample - ditherMask.
			green < 1 ifTrue: [ green := 1 ].
			blue := y + (FIXn1n77200 * cb // 65536) + (residuals at: 3).
			blue > MaxSample 
				ifTrue: [ blue := MaxSample ]
				ifFalse: [ blue < 0 ifTrue: [ blue := 0 ] ].
			residuals 
				at: 3
				put: (blue bitAnd: ditherMask).
			blue := blue bitAnd: MaxSample - ditherMask.
			blue < 1 ifTrue: [ blue := 1 ].
			bits 
				at: i
				put: 4278190080 + (red bitShift: 16) + (green bitShift: 8) + blue ]
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> colorConvertMCU [

	^ currentComponents size = 3
		ifTrue:
			[self useFloatingPoint
				ifTrue: [self colorConvertFloatYCbCrMCU]
				ifFalse: [self primColorConvertYCbCrMCU: currentComponents
								bits: mcuImageBuffer bits
								residuals: residuals
								ditherMask: ditherMask.]]
		ifFalse: [self primColorConvertGrayscaleMCU]
]

{ #category : #dct }
JPEGReadWriter >> dctFloatRangeLimit: value [

	^ (value / 8.0) + FloatSampleOffset.
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> decodeBlockInto: anArray component: aColorComponent dcTable: huffmanDC acTable: huffmanAC [ 
	| byte i zeroCount |
	byte := stream decodeValueFrom: huffmanDC.
	byte ~= 0 ifTrue: 
		[ byte := self 
			scaleAndSignExtend: (self getBits: byte)
			inFieldWidth: byte ].
	byte := aColorComponent updateDCValue: byte.
	anArray atAllPut: 0.
	anArray 
		at: 1
		put: byte.
	i := 2.
	[ i <= DCTSize2 ] whileTrue: 
		[ byte := stream decodeValueFrom: huffmanAC.
		zeroCount := byte >> 4.
		byte := byte bitAnd: 15.
		byte ~= 0 
			ifTrue: 
				[ i := i + zeroCount.
				byte := self 
					scaleAndSignExtend: (self getBits: byte)
					inFieldWidth: byte.
				anArray 
					at: (JPEGNaturalOrder at: i)
					put: byte ]
			ifFalse: 
				[ zeroCount = 15 
					ifTrue: [ i := i + zeroCount ]
					ifFalse: [ ^ self ] ].
		i := i + 1 ]
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> decodeMCU [
	| comp ci |
	(restartInterval ~= 0 and: [ restartsToGo = 0 ]) ifTrue: [ self processRestart ].
	1 
		to: mcuMembership size
		do: 
			[ :i | 
			ci := mcuMembership at: i.
			comp := currentComponents at: ci.
			self 
				primDecodeBlockInto: (mcuSampleBuffer at: i)
				component: comp
				dcTable: (hDCTable at: comp dcTableIndex)
				acTable: (hACTable at: comp acTableIndex)
				stream: stream ].
	restartsToGo := restartsToGo - 1
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> getBits: requestedBits [
	^stream getBits: requestedBits
]

{ #category : #accessing }
JPEGReadWriter >> hACTable [
	hACTable ifNil: [ hACTable := Array new: HuffmanTableSize ].
	^ hACTable
]

{ #category : #accessing }
JPEGReadWriter >> hDCTable [
	hDCTable ifNil: [ hDCTable := Array new: HuffmanTableSize ].
	^ hDCTable
]

{ #category : #dct }
JPEGReadWriter >> idctBlockFloat: anArray component: aColorComponent [ 
	| t0 t1 t2 t3 t4 t5 t6 t7 t10 t11 t12 t13 z5 z10 z11 z12 z13 qt ws |
	qt := self qTable at: aColorComponent qTableIndex.
	ws := Array new: DCTSize2.

	"Pass 1: process columns from input, store into work array"
	1 
		to: DCTSize
		do: 
			[ :i | 
			t0 := (anArray at: i) * (qt at: i).
			t1 := (anArray at: DCTSize * 2 + i) * (qt at: DCTSize * 2 + i).
			t2 := (anArray at: DCTSize * 4 + i) * (qt at: DCTSize * 4 + i).
			t3 := (anArray at: DCTSize * 6 + i) * (qt at: DCTSize * 6 + i).
			t10 := t0 + t2.
			t11 := t0 - t2.
			t13 := t1 + t3.
			t12 := (t1 - t3) * DCTK1 - t13.
			t0 := t10 + t13.
			t3 := t10 - t13.
			t1 := t11 + t12.
			t2 := t11 - t12.
			t4 := (anArray at: DCTSize + i) * (qt at: DCTSize + i).
			t5 := (anArray at: DCTSize * 3 + i) * (qt at: DCTSize * 3 + i).
			t6 := (anArray at: DCTSize * 5 + i) * (qt at: DCTSize * 5 + i).
			t7 := (anArray at: DCTSize * 7 + i) * (qt at: DCTSize * 7 + i).
			z13 := t6 + t5.
			z10 := t6 - t5.
			z11 := t4 + t7.
			z12 := t4 - t7.
			t7 := z11 + z13.
			t11 := (z11 - z13) * DCTK1.
			z5 := (z10 + z12) * DCTK2.
			t10 := DCTK3 * z12 - z5.
			t12 := DCTK4 * z10 + z5.
			t6 := t12 - t7.
			t5 := t11 - t6.
			t4 := t10 + t5.
			ws 
				at: i
				put: t0 + t7.
			ws 
				at: DCTSize * 7 + i
				put: t0 - t7.
			ws 
				at: DCTSize + i
				put: t1 + t6.
			ws 
				at: DCTSize * 6 + i
				put: t1 - t6.
			ws 
				at: DCTSize * 2 + i
				put: t2 + t5.
			ws 
				at: DCTSize * 5 + i
				put: t2 - t5.
			ws 
				at: DCTSize * 4 + i
				put: t3 + t4.
			ws 
				at: DCTSize * 3 + i
				put: t3 - t4 ].

	"Pass 2: process rows from the workspace"
	(0 
		to: DCTSize2 - DCTSize
		by: DCTSize) do: 
		[ :i | 
		t10 := (ws at: i + 1) + (ws at: i + 5).
		t11 := (ws at: i + 1) - (ws at: i + 5).
		t13 := (ws at: i + 3) + (ws at: i + 7).
		t12 := ((ws at: i + 3) - (ws at: i + 7)) * DCTK1 - t13.
		t0 := t10 + t13.
		t3 := t10 - t13.
		t1 := t11 + t12.
		t2 := t11 - t12.
		z13 := (ws at: i + 6) + (ws at: i + 4).
		z10 := (ws at: i + 6) - (ws at: i + 4).
		z11 := (ws at: i + 2) + (ws at: i + 8).
		z12 := (ws at: i + 2) - (ws at: i + 8).
		t7 := z11 + z13.
		t11 := (z11 - z13) * DCTK1.
		z5 := (z10 + z12) * DCTK2.
		t10 := DCTK3 * z12 - z5.
		t12 := DCTK4 * z10 + z5.
		t6 := t12 - t7.
		t5 := t11 - t6.
		t4 := t10 + t5.

		"final output stage: scale down by a factor of 8 and range-limit"
		anArray 
			at: i + 1
			put: (self dctFloatRangeLimit: t0 + t7).
		anArray 
			at: i + 8
			put: (self dctFloatRangeLimit: t0 - t7).
		anArray 
			at: i + 2
			put: (self dctFloatRangeLimit: t1 + t6).
		anArray 
			at: i + 7
			put: (self dctFloatRangeLimit: t1 - t6).
		anArray 
			at: i + 3
			put: (self dctFloatRangeLimit: t2 + t5).
		anArray 
			at: i + 6
			put: (self dctFloatRangeLimit: t2 - t5).
		anArray 
			at: i + 5
			put: (self dctFloatRangeLimit: t3 + t4).
		anArray 
			at: i + 4
			put: (self dctFloatRangeLimit: t3 - t4) ]
]

{ #category : #dct }
JPEGReadWriter >> idctBlockInt: anArray component: aColorComponent [
	^self idctBlockInt: anArray qt: (self qTable at: aColorComponent qTableIndex)
]

{ #category : #dct }
JPEGReadWriter >> idctBlockInt: anArray qt: qt [ 
	| ws anACTerm dcval z1 z2 z3 z4 z5 t0 t1 t2 t3 t10 t11 t12 t13 shift |
	ws := Array new: DCTSize2.

	"Pass 1: process columns from anArray, store into work array"
	shift := 1 bitShift: ConstBits - Pass1Bits.
	1 
		to: DCTSize
		do: 
			[ :i | 
			anACTerm := nil.
			1 
				to: DCTSize - 1
				do: 
					[ :row | 
					anACTerm ifNil: [ (anArray at: row * DCTSize + i) = 0 ifFalse: [ anACTerm := row ] ] ].
			anACTerm == nil 
				ifTrue: 
					[ dcval := (anArray at: i) * (qt at: 1) bitShift: Pass1Bits.
					0 
						to: DCTSize - 1
						do: 
							[ :j | 
							ws 
								at: j * DCTSize + i
								put: dcval ] ]
				ifFalse: 
					[ z2 := (anArray at: DCTSize * 2 + i) * (qt at: DCTSize * 2 + i).
					z3 := (anArray at: DCTSize * 6 + i) * (qt at: DCTSize * 6 + i).
					z1 := (z2 + z3) * FIXn0n541196100.
					t2 := z1 + (z3 * FIXn1n847759065 negated).
					t3 := z1 + (z2 * FIXn0n765366865).
					z2 := (anArray at: i) * (qt at: i).
					z3 := (anArray at: DCTSize * 4 + i) * (qt at: DCTSize * 4 + i).
					t0 := z2 + z3 bitShift: ConstBits.
					t1 := z2 - z3 bitShift: ConstBits.
					t10 := t0 + t3.
					t13 := t0 - t3.
					t11 := t1 + t2.
					t12 := t1 - t2.
					t0 := (anArray at: DCTSize * 7 + i) * (qt at: DCTSize * 7 + i).
					t1 := (anArray at: DCTSize * 5 + i) * (qt at: DCTSize * 5 + i).
					t2 := (anArray at: DCTSize * 3 + i) * (qt at: DCTSize * 3 + i).
					t3 := (anArray at: DCTSize + i) * (qt at: DCTSize + i).
					z1 := t0 + t3.
					z2 := t1 + t2.
					z3 := t0 + t2.
					z4 := t1 + t3.
					z5 := (z3 + z4) * FIXn1n175875602.
					t0 := t0 * FIXn0n298631336.
					t1 := t1 * FIXn2n053119869.
					t2 := t2 * FIXn3n072711026.
					t3 := t3 * FIXn1n501321110.
					z1 := z1 * FIXn0n899976223 negated.
					z2 := z2 * FIXn2n562915447 negated.
					z3 := z3 * FIXn1n961570560 negated.
					z4 := z4 * FIXn0n390180644 negated.
					z3 := z3 + z5.
					z4 := z4 + z5.
					t0 := t0 + z1 + z3.
					t1 := t1 + z2 + z4.
					t2 := t2 + z2 + z3.
					t3 := t3 + z1 + z4.
					ws 
						at: i
						put: t10 + t3 >> (ConstBits - Pass1Bits).
					ws 
						at: DCTSize * 7 + i
						put: (t10 - t3) // shift.
					ws 
						at: DCTSize * 1 + i
						put: (t11 + t2) // shift.
					ws 
						at: DCTSize * 6 + i
						put: (t11 - t2) // shift.
					ws 
						at: DCTSize * 2 + i
						put: (t12 + t1) // shift.
					ws 
						at: DCTSize * 5 + i
						put: (t12 - t1) // shift.
					ws 
						at: DCTSize * 3 + i
						put: (t13 + t0) // shift.
					ws 
						at: DCTSize * 4 + i
						put: (t13 - t0) // shift ] ].

	"Pass 2: process rows from work array, store back into anArray"
	shift := 1 bitShift: ConstBits + Pass1Bits + 3.
	0 
		to: DCTSize2 - DCTSize
		by: DCTSize
		do: 
			[ :i | 
			z2 := ws at: i + 3.
			z3 := ws at: i + 7.
			z1 := (z2 + z3) * FIXn0n541196100.
			t2 := z1 + (z3 * FIXn1n847759065 negated).
			t3 := z1 + (z2 * FIXn0n765366865).
			t0 := (ws at: i + 1) + (ws at: i + 5) bitShift: ConstBits.
			t1 := (ws at: i + 1) - (ws at: i + 5) bitShift: ConstBits.
			t10 := t0 + t3.
			t13 := t0 - t3.
			t11 := t1 + t2.
			t12 := t1 - t2.
			t0 := ws at: i + 8.
			t1 := ws at: i + 6.
			t2 := ws at: i + 4.
			t3 := ws at: i + 2.
			z1 := t0 + t3.
			z2 := t1 + t2.
			z3 := t0 + t2.
			z4 := t1 + t3.
			z5 := (z3 + z4) * FIXn1n175875602.
			t0 := t0 * FIXn0n298631336.
			t1 := t1 * FIXn2n053119869.
			t2 := t2 * FIXn3n072711026.
			t3 := t3 * FIXn1n501321110.
			z1 := z1 * FIXn0n899976223 negated.
			z2 := z2 * FIXn2n562915447 negated.
			z3 := z3 * FIXn1n961570560 negated.
			z4 := z4 * FIXn0n390180644 negated.
			z3 := z3 + z5.
			z4 := z4 + z5.
			t0 := t0 + z1 + z3.
			t1 := t1 + z2 + z4.
			t2 := t2 + z2 + z3.
			t3 := t3 + z1 + z4.
			anArray 
				at: i + 1
				put: (self sampleRangeLimit: (t10 + t3) // shift + SampleOffset).
			anArray 
				at: i + 8
				put: (self sampleRangeLimit: (t10 - t3) // shift + SampleOffset).
			anArray 
				at: i + 2
				put: (self sampleRangeLimit: (t11 + t2) // shift + SampleOffset).
			anArray 
				at: i + 7
				put: (self sampleRangeLimit: (t11 - t2) // shift + SampleOffset).
			anArray 
				at: i + 3
				put: (self sampleRangeLimit: (t12 + t1) // shift + SampleOffset).
			anArray 
				at: i + 6
				put: (self sampleRangeLimit: (t12 - t1) // shift + SampleOffset).
			anArray 
				at: i + 4
				put: (self sampleRangeLimit: (t13 + t0) // shift + SampleOffset).
			anArray 
				at: i + 5
				put: (self sampleRangeLimit: (t13 - t0) // shift + SampleOffset) ]
]

{ #category : #dct }
JPEGReadWriter >> idctMCU [
	| comp fp ci |
	fp := self useFloatingPoint.
	1 
		to: mcuMembership size
		do: 
			[ :i | 
			ci := mcuMembership at: i.
			comp := currentComponents at: ci.
			fp 
				ifTrue: 
					[ self 
						idctBlockFloat: (mcuSampleBuffer at: i)
						component: comp ]
				ifFalse: 
					[ self 
						primIdctInt: (mcuSampleBuffer at: i)
						qt: (qTable at: comp qTableIndex) ] ]
]

{ #category : #'marker parsing' }
JPEGReadWriter >> initialSOSSetup [
	mcuWidth := (components detectMax: [ :c | c widthInBlocks ]) widthInBlocks.
	mcuHeight := (components detectMax: [ :c | c heightInBlocks ]) heightInBlocks.
	components do: 
		[ :c | 
		c 
			mcuWidth: mcuWidth
			mcuHeight: mcuHeight
			dctSize: DCTSize ].
	stream resetBitBuffer
]

{ #category : #'public access' }
JPEGReadWriter >> nextImage [

	^ self nextImageDitheredToDepth: Display depth

]

{ #category : #'public access' }
JPEGReadWriter >> nextImageDitheredToDepth: depth [ 
	| form xStep yStep x y bb |
	ditherMask := DitherMasks 
		at: depth
		ifAbsent: [ self error: 'can only dither to display depths' ].
	residuals := WordArray new: 3.
	sosSeen := false.
	self parseFirstMarker.
	[ sosSeen ] whileFalse: [ self parseNextMarker ].
	form := Form 
		extent: width @ height
		depth: depth.
	bb := BitBlt toForm: form.
	bb sourceForm: mcuImageBuffer.
	bb colorMap: (mcuImageBuffer colormapIfNeededFor: form).
	bb sourceRect: mcuImageBuffer boundingBox.
	bb combinationRule: Form over.
	xStep := mcuWidth * DCTSize.
	yStep := mcuHeight * DCTSize.
	y := 0.
	1 
		to: mcuRowsInScan
		do: 
			[ :row | 
			x := 0.
			1 
				to: mcusPerRow
				do: 
					[ :col | 
					self decodeMCU.
					self idctMCU.
					self colorConvertMCU.
					bb
						destX: x;
						destY: y;
						copyBits.
					x := x + xStep ].
			y := y + yStep ].
	^ form
]

{ #category : #'public access' }
JPEGReadWriter >> nextPutImage: aForm [
	self error: 'JPEGReadWriter supports decoding, for encoding, look at PluginBasedJPEGReadWriter'
]

{ #category : #'error handling' }
JPEGReadWriter >> notSupported: aString [

	self error: aString , ' is not currently supported'
]

{ #category : #'marker parsing' }
JPEGReadWriter >> okToIgnoreMarker: aMarker [

	^ (((16rE0 to: 16rEF) includes: aMarker) "unhandled APPn markers"
		or: [aMarker = 16rDC or: [aMarker = 16rFE]]) "DNL or COM markers"
		or: [aMarker = 16r99] "Whatever that is"
]

{ #category : #private }
JPEGReadWriter >> on: aStream [ 
	super on: aStream.
	stream := JPEGReadStream on: stream upToEnd
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseAPPn [
	| length buffer thumbnailLength markerStart |
	markerStart := self position.
	length := self nextWord.
	buffer := self next: 4.
	buffer asString = 'JFIF' ifFalse: 
		[ "Skip APPs that we're not interested in"
		stream next: length - 6.
		^ self ].
	self next.
	majorVersion := self next.
	minorVersion := self next.
	densityUnit := self next.
	xDensity := self nextWord.
	yDensity := self nextWord.
	thumbnailLength := self next * self next * 3.
	length := length - (self position - markerStart).
	length = thumbnailLength ifFalse: [ self error: 'APP0 thumbnail length is incorrect.' ].
	self next: length
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseDecoderRestartInterval [
	| length |
	length := self nextWord.
	length = 4 ifFalse: [ self error: 'DRI length incorrect' ].
	restartInterval := self nextWord
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseFirstMarker [
	| marker |
	self next = 255 ifFalse: [ self error: 'JFIF marker expected' ].
	marker := self next.
	marker = 217 ifTrue: [ ^ self	"halt: 'EOI encountered.'" ].
	marker = 216 ifFalse: [ self error: 'SOI marker expected' ].
	self parseStartOfInput
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseHuffmanTable [
	| length markerStart index bits count huffVal isACTable hTable |
	markerStart := self position.
	length := self nextWord.
	[ self position - markerStart >= length ] whileFalse: 
		[ index := self next.
		isACTable := (index bitAnd: 16) ~= 0.
		index := (index bitAnd: 15) + 1.
		index > HuffmanTableSize ifTrue: 
			[ self error: 'image has more than ' , HuffmanTableSize printString , ' quantization tables' ].
		bits := self next: 16.
		count := bits sum.
		(count > 256 or: [ count > (length - (self position - markerStart)) ]) ifTrue: [ self error: 'Huffman Table count is incorrect' ].
		huffVal := self next: count.
		hTable := stream 
			buildLookupTable: huffVal
			counts: bits.
		isACTable 
			ifTrue: 
				[ self hACTable 
					at: index
					put: hTable ]
			ifFalse: 
				[ self hDCTable 
					at: index
					put: hTable ] ]
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseNOP [

	"don't need to do anything, here"
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseNextMarker [
	"Parse the next marker of the stream"
	| byte discardedBytes |
	discardedBytes := 0.
	[ (byte := self next) = 255 ] whileFalse: [ discardedBytes := discardedBytes + 1 ].
	
	[ [ (byte := self next) = 255 ] whileTrue.
	byte = 0 ] whileTrue: [ discardedBytes := discardedBytes + 2 ].
	discardedBytes > 0 ifTrue: 
		[ "notifyWithLabel: 'warning: extraneous data discarded'"
		self ].
	self perform: (JFIFMarkerParser 
			at: byte
			ifAbsent: 
				[ (self okToIgnoreMarker: byte) 
					ifTrue: [ #skipMarker ]
					ifFalse: [ self error: 'marker ' , byte printStringHex , ' cannot be handled' ] ])
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseQuantizationTable [
	| length markerStart n prec value table |
	markerStart := self position.
	length := self nextWord.
	[ self position - markerStart >= length ] whileFalse: 
		[ value := self next.
		n := (value bitAnd: 15) + 1.
		prec := value >> 4 > 0.
		n > QuantizationTableSize ifTrue: 
			[ self error: 'image has more than ' , QuantizationTableSize printString , ' quantization tables' ].
		table := IntegerArray new: DCTSize2.
		1 
			to: DCTSize2
			do: 
				[ :i | 
				value := prec 
					ifTrue: [ self nextWord ]
					ifFalse: [ self next ].
				table 
					at: (JPEGNaturalOrder at: i)
					put: value ].
		self useFloatingPoint ifTrue: [ self scaleQuantizationTable: table ].
		self qTable 
			at: n
			put: table ]
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseStartOfFile [
	| length markerStart value n |
	markerStart := self position.
	length := self nextWord.
	dataPrecision := self next.
	dataPrecision = 8 ifFalse: 
		[ self error: 'cannot handle ' , dataPrecision printString , '-bit components' ].
	height := self nextWord.
	width := self nextWord.
	n := self next.
	height = 0 | (width = 0) | (n = 0) ifTrue: [ self error: 'empty image' ].
	length - (self position - markerStart) ~= (n * 3) ifTrue: [ self error: 'component length is incorrect' ].
	components := Array new: n.
	1 
		to: components size
		do: 
			[ :i | 
			components 
				at: i
				put: (JPEGColorComponent new
						id: self next;
						widthInBlocks: ((value := self next) >> 4 bitAnd: 15);
						heightInBlocks: (value bitAnd: 15);
						qTableIndex: self next + 1)
			"heightInBlocks: (((value := self next) >> 4) bitAnd: 16r0F);
					widthInBlocks: (value bitAnd: 16r0F);" ]
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseStartOfInput [
	restartInterval := 0.
	densityUnit := 0.
	xDensity := 1.
	yDensity := 1
]

{ #category : #'marker parsing' }
JPEGReadWriter >> parseStartOfScan [
	| length n id value dcNum acNum comp |
	length := self nextWord.
	n := self next.
	length ~= (n * 2 + 6) | (n < 1) ifTrue: [ self error: 'SOS length is incorrect' ].
	currentComponents := Array new: n.
	1 
		to: n
		do: 
			[ :i | 
			id := self next.
			value := self next.
			dcNum := value >> 4 bitAnd: 15.
			acNum := value bitAnd: 15.
			comp := components detect: [ :c | c id = id ].
			comp
				dcTableIndex: dcNum + 1;
				acTableIndex: acNum + 1.
			currentComponents 
				at: i
				put: comp ].
	ss := self next.
	se := self next.
	value := self next.
	ah := value >> 4 bitAnd: 15.
	al := value bitAnd: 15.
	self initialSOSSetup.
	self perScanSetup.
	sosSeen := true
]

{ #category : #'marker parsing' }
JPEGReadWriter >> perScanSetup [
	mcusPerRow := (width / (mcuWidth * DCTSize)) ceiling.
	mcuRowsInScan := (height / (mcuHeight * DCTSize)) ceiling.
	(currentComponents size = 3 or: [ currentComponents size = 1 ]) ifFalse: [ self error: 'JPEG color space not recognized' ].
	mcuMembership := OrderedCollection new.
	currentComponents withIndexDo: 
		[ :c :i | 
		c priorDCValue: 0.
		mcuMembership addAll: ((1 to: c totalMcuBlocks) collect: [ :b | i ]) ].
	mcuMembership := mcuMembership asArray.
	mcuSampleBuffer := (1 to: mcuMembership size) collect: [ :i | IntegerArray new: DCTSize2 ].
	currentComponents withIndexDo: 
		[ :c :i | 
		c initializeSampleStreamBlocks: ((1 to: mcuMembership size) 
				select: [ :j | i = (mcuMembership at: j) ]
				thenCollect: [ :j | mcuSampleBuffer at: j ]) ].
	mcuImageBuffer := Form 
		extent: mcuWidth @ mcuHeight * DCTSize
		depth: 32.
	restartsToGo := restartInterval
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> primColorConvertGrayscaleMCU [
	self primColorConvertGrayscaleMCU: (currentComponents at: 1)
			bits: mcuImageBuffer bits
			residuals: residuals
			ditherMask: ditherMask.
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> primColorConvertGrayscaleMCU: componentArray bits: bits residuals: residualArray ditherMask: mask [
	<primitive: 'primitiveColorConvertGrayscaleMCU' module: 'JPEGReaderPlugin'>
	"JPEGReaderPlugin doPrimitive: #primitiveColorConvertGrayscaleMCU."
	^self colorConvertGrayscaleMCU
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> primColorConvertIntYCbCrMCU [
	self primColorConvertYCbCrMCU: currentComponents
			bits: mcuImageBuffer bits
			residuals: residuals
			ditherMask: ditherMask.
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> primColorConvertYCbCrMCU: componentArray bits: bits residuals: residualArray ditherMask: mask [
	<primitive: 'primitiveColorConvertMCU' module: 'JPEGReaderPlugin'>
	^self colorConvertIntYCbCrMCU
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> primDecodeBlockInto: sampleBuffer component: comp dcTable: dcTable acTable: acTable stream: jpegStream [
	<primitive: 'primitiveDecodeMCU' module: 'JPEGReaderPlugin'>
	^self decodeBlockInto: sampleBuffer component: comp dcTable: dcTable acTable: acTable
]

{ #category : #dct }
JPEGReadWriter >> primIdctBlockInt: anArray component: aColorComponent [
	^self primIdctInt: anArray qt: (self qTable at: aColorComponent qTableIndex)
]

{ #category : #dct }
JPEGReadWriter >> primIdctInt: anArray qt: qt [
	<primitive: 'primitiveIdctInt' module: 'JPEGReaderPlugin'>
	^self idctBlockInt: anArray qt: qt
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> processRestart [
	stream resetBitBuffer.
	self parseNextMarker.
	currentComponents do: [ :c | c priorDCValue: 0 ].
	restartsToGo := restartInterval
]

{ #category : #accessing }
JPEGReadWriter >> qTable [
	qTable ifNil: [ qTable := Array new: QuantizationTableSize ].
	^ qTable
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> sampleFloatRangeLimit: aNumber [

	^ (aNumber rounded max: 0) min: MaxSample
]

{ #category : #'colorspace conversion' }
JPEGReadWriter >> sampleRangeLimit: aNumber [

	aNumber < 0 ifTrue: [^ 0].
	aNumber > MaxSample ifTrue: [^ MaxSample].
	^ aNumber
]

{ #category : #'huffman encoding' }
JPEGReadWriter >> scaleAndSignExtend: aNumber inFieldWidth: w [

	aNumber < (1 bitShift: (w - 1))
		ifTrue: [^aNumber - (1 bitShift: w) + 1]
		ifFalse: [^aNumber]
]

{ #category : #dct }
JPEGReadWriter >> scaleQuantizationTable: table [ 
	| index |
	index := 1.
	1 
		to: DCTSize
		do: 
			[ :row | 
			1 
				to: DCTSize
				do: 
					[ :col | 
					table 
						at: index
						put: ((table at: index) * (QTableScaleFactor at: row) * (QTableScaleFactor at: col)) rounded.
					index := index + 1 ] ].
	^ table
]

{ #category : #'public access' }
JPEGReadWriter >> setStream: aStream [ 
	"Feed it in from an existing source"
	stream := JPEGReadStream on: aStream upToEnd
]

{ #category : #'marker parsing' }
JPEGReadWriter >> skipMarker [
	| length markerStart |
	markerStart := self position.
	length := self nextWord.
	self next: length - (self position - markerStart)
]

{ #category : #testing }
JPEGReadWriter >> understandsImageFormat [
	"Answer true if the image stream format is understood by this decoder."
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true

]

{ #category : #preferences }
JPEGReadWriter >> useFloatingPoint [

	^ false
]
