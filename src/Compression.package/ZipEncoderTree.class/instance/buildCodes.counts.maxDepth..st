encoding
buildCodes: nodeList counts: blCounts maxDepth: depth
	"Build the codes for all nodes"
	| nextCode code node length |
	nextCode := WordArray new: depth+1.
	code := 0.
	1 to: depth do:[:bits|
		code := (code + (blCounts at: bits)) << 1.
		nextCode at: bits+1 put: code].
	[(code + (blCounts at: depth+1) - 1) = (1 << depth - 1)] assert.
	0 to: maxCode do:[:n|
		node := nodeList at: n+1.
		length := node bitLength.
		length = 0 ifFalse:[
			code := nextCode at: length+1.
			node code: (self reverseBits: code length: length).
			nextCode at: length+1 put: code+1.
		].
	].