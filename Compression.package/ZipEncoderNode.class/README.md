ZipEncoderNode represents a node in a huffman tree for encoding ZipStreams.

Instance variables:
	value 		<Integer>	- Encoded value
	frequency	<Integer>	- Number of occurences of the encoded value
	height 		<Integer>	- Height of the node in the tree
	bitLength 	<Integer>	- bit length of the code
	code		<Integer>	- Assigned code for this node
	parent		<ZipEncoderNode>		- Parent of this node
	left			<ZipEncoderNode>		- First child of this node
	right		<ZipEncoderNode>		- Second child of this node
