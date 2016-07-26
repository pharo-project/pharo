encoding
litIndex: literal
	| p |
	p := literalStream position.
	p = 256 ifTrue:
		[self notify: 'More than 256 literals referenced. 
You must split or otherwise simplify this method.
The 257th literal is: ', literal printString. ^nil].
		"Would like to show where it is in the source code, 
		 but that info is hard to get."
	literalStream nextPut: literal.
	^ p