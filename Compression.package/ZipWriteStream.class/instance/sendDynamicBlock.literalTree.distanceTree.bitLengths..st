dynamic blocks
sendDynamicBlock: blTree literalTree: lTree distanceTree: dTree bitLengths: bits
	"Send a block using dynamic huffman trees"
	self sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits.
	self sendCompressedBlock: lTree with: dTree.