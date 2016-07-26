huffman trees
growHuffmanTable: table
	| newTable |
	newTable := table species new: table size * 2.
	newTable replaceFrom: 1 to: table size with: table startingAt: 1.
	^newTable