control flow instructions
decodeLoops: loops
	"There are multiple loops if all their backjumps target the same pc.
	In this case the loops are ordereed from innerMost to outerMost, and we decompile them in the correct order so the inner loop is decompiled inside the outerloop"
	self decodeLoop: loops removeLast nextLoops: loops