encoding
encodeBitLength: blCounts from: aTree
	| index |
	"Note: If bitLength is not nil then the tree must be broken"
	bitLength isNil ifFalse:[self error:'Huffman tree is broken'].
	parent  
		ifNil: [bitLength := 0]
		ifNotNil:[bitLength := parent bitLength + 1].
	self isLeaf ifTrue:[
		index := bitLength + 1.
		blCounts at: index put: (blCounts at: index) + 1.
	] ifFalse:[
		left encodeBitLength: blCounts from: aTree.
		right encodeBitLength: blCounts from: aTree.
	].