pool initialization
initializeExtraBits
	ExtraLengthBits := 
		WordArray withAll: #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0).
	ExtraDistanceBits := 
		WordArray withAll: #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	ExtraBitLengthBits := 
		WordArray withAll: #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 7).
	BitLengthOrder :=
		WordArray withAll: #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15).
