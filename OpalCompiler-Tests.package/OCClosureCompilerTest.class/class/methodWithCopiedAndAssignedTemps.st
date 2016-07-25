code examples
methodWithCopiedAndAssignedTemps
	| blk "0w" a "0w" b "0w" c "0w" t "0w" r1 "0w" r2 "0w" |
	a := 1. "1w"
	b := 2. "1w"
	c := 4. "1w"
	t := 0. "1w"
	blk "5w" := ["2" t  "3w" := t "3r" + a "3r" + b "3r" + c "3r" ] "4".
	r1 "5w" := blk "5r" value.
	b "5w" := -100.
	r2 "5w" := blk "5r" value.
	^r1 "5r" -> r2 "5r" -> t "5r"

	"a: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 b: main(read(),write(0,1,5)), block(read(3),write()) => remote; write follows contained read
	 blk: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 c: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 r1: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 r2: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 t: main(read(5),write(0,1)), block(read(3),write(3)) => remote; read follows contained write"


	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"