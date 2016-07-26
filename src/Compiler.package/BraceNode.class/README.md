Used for compiling and decompiling brace constructs.

These now compile into either a fast short form for 4 elements or less:
	Array braceWith: a with: b ... 
or a long form of indefinfite length:
	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.

The erstwhile brace assignment form is no longer supported.