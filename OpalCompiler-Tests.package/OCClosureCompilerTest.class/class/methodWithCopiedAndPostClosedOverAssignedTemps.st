code examples
methodWithCopiedAndPostClosedOverAssignedTemps
	| blk a b c r1 r2 |
	a := 1.
	b := 2.
	c := 4.
	blk := [a + b + c].
	r1 := blk value.
	b := nil.
	r2 := blk value.
	r1 -> r2

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndPostClosedOverAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"