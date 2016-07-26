fixed blocks
sendFixedBlock
	"Send a block using fixed huffman trees"
	self sendCompressedBlock: FixedLiteralTree with: FixedDistanceTree.