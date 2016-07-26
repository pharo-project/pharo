dynamic blocks
sendCompressedBlock: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| sum |
	sum := encoder
			sendBlock: (ReadStream on: literals from: 1 to: litCount)
			with: (ReadStream on: distances from: 1 to: litCount)
			with: litTree
			with: distTree.
	sum = (blockPosition - blockStart) ifFalse:[self error:'Wrong number of bytes'].