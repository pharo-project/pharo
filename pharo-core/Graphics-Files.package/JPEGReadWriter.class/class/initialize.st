initialization
initialize
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