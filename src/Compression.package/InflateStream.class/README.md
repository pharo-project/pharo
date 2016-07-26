This class implements the Inflate decompression algorithm as defined by RFC1951 and used in PKZip, GZip and ZLib (and many, many more). It is a variant of the LZ77 compression algorithm described in

[LZ77] Ziv J., Lempel A., "A Universal Algorithm for Sequential Data Compression", IEEE Transactions on Information Theory", Vol. 23, No. 3, pp. 337-343.

[RFC1951] Deutsch. P, "DEFLATE Compressed Data Format Specification version 1.3"

For more information see the above mentioned RFC 1951 which can for instance be found at

	http://www.leo.org/pub/comp/doc/standards/rfc/index.html

Huffman Tree Implementation Notes:
===========================================
The huffman tree used for decoding literal, distance and length codes in the inflate algorithm has been encoded in a single Array. The tree is made up of subsequent tables storing all entries at the current bit depth. Each entry in the table (e.g., a 32bit Integer value) is either a leaf or a non-leaf node. Leaf nodes store the immediate value in its low 16 bits whereas non-leaf nodes store the offset of the subtable in its low 16bits. The high 8 bits of non-leaf nodes contain the number of additional bits needed for the sub table (the high 8 bits of leaf-nodes are always zero). The first entry in each table is always a non-leaf node indicating how many bits we need to fetch initially. We can thus travel down the tree as follows (written in sort-of-pseudocode the actual implementation can be seen in InflateStream>>decodeValueFrom:):

	table := initialTable.
	bitsNeeded := high 8 bits of (table at: 1).		"Determine initial bits"
	table := initialTable + (low 16 bits of (table at: 1)). "Determine start of first real table"
	[bits := fetch next bitsNeeded bits.			"Grab the bits"
	value := table at: bits.						"Lookup the value"
	value has high 8 bit set] whileTrue:[		"Check if it's leaf"
		table := initialTable + (low 16 bits of value).	"No - compute new sub table start"
		bitsNeeded := high 8 bit of value].		"Compute additional number of bits needed"
	^value
