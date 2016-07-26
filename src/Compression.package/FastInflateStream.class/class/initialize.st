initialization
initialize
	"FastInflateStream initialize"
	| low high |

	"Init literal/length map"
	low := #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 ).
	high := #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0).
	LiteralLengthMap := WordArray new: 256 + 32.
	1 to: 257 do:[:i| LiteralLengthMap at: i put: i-1].
	1 to: 29 do:[:i| LiteralLengthMap at: 257+i put: (low at:i) + ( (high at: i) + 1 << 16)].

	"Init distance map"
	high := #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	low := #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
			1025 1537 2049 3073 4097 6145 8193 12289 16385 24577).
	DistanceMap := WordArray new: 32.
	1 to: 30 do:[:i| DistanceMap at: i put: (low at: i) + ( (high at: i) << 16)].

	"Init fixed block huffman tables"
	FixedLitTable := self basicNew
				huffmanTableFrom: FixedLitCodes
				mappedBy: LiteralLengthMap.
	FixedDistTable := self basicNew
				huffmanTableFrom: FixedDistCodes
				mappedBy: DistanceMap.