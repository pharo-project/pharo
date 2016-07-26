reading
readHeader
	| reserved |
	bfType := stream nextLittleEndianNumber: 2.
	bfSize := stream nextLittleEndianNumber: 4.
	reserved := stream nextLittleEndianNumber: 4.
	bfOffBits := stream nextLittleEndianNumber: 4.
	biSize := stream nextLittleEndianNumber: 4.
	biWidth := stream nextLittleEndianNumber: 4.
	biHeight := stream nextLittleEndianNumber: 4.
	biPlanes := stream nextLittleEndianNumber: 2.
	biBitCount := stream nextLittleEndianNumber: 2.
	biCompression := stream nextLittleEndianNumber: 4.
	biSizeImage := stream nextLittleEndianNumber: 4.
	biXPelsPerMeter := stream nextLittleEndianNumber: 4.
	biYPelsPerMeter := stream nextLittleEndianNumber: 4.
	biClrUsed := stream nextLittleEndianNumber: 4.
	biClrImportant := stream nextLittleEndianNumber: 4