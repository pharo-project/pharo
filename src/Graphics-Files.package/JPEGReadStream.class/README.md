Encapsulates huffman encoded access to JPEG data.

The following layout is fixed for the JPEG primitives to work:

	collection	<ByteArray | String>
	position		<SmallInteger>
	readLimit	<SmallInteger>
	bitBuffer	<SmallInteger>
	bitsInBuffer	<SmallInteger>