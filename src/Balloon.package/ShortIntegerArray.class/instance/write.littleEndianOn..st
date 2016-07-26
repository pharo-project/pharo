objects from disk
write: shortInteger littleEndianOn: aStream

	#(2 1 4 3) do: [:digitIndex| aStream nextPut: (shortInteger digitAt: digitIndex) ]