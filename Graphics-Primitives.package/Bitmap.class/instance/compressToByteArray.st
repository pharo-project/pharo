filing
compressToByteArray
	"Return a run-coded compression of this bitmap into a byteArray"
	"Without skip codes, it is unlikely that the compressed bitmap will be any larger than was the original.  The run-code cases are...
	N >= 1 words of equal bytes:  4N bytes -> 2 bytes (at worst 4 -> 2)
	N > 1 equal words:  4N bytes -> 5 bytes (at worst 8 -> 5)
	N > 1 unequal words:  4N bytes -> 4N + M, where M is the number of bytes required to encode the run length.

The worst that can happen is that the method begins with unequal words, and than has interspersed occurrences of a word with equal bytes.  Thus we require a run-length at the beginning, and after every interspersed word of equal bytes.  However, each of these saves 2 bytes, so it must be followed by a run of 1984 (7936//4) or more (for which M jumps from 2 to 5) to add any extra overhead.  Therefore the worst case is a series of runs of 1984 or more, with single interspersed words of equal bytes.  At each break we save 2 bytes, but add 5.  Thus the overhead would be no more than 5 (encoded size) + 2 (first run len) + (S//1984*3)."
	"NOTE: This code is copied in Form hibernate for reasons given there."
	| byteArray lastByte |
	byteArray := ByteArray new: self size * 4 + 7 + (self size // 1984 * 3).
	lastByte := self 
		compress: self
		toByteArray: byteArray.
	^ byteArray 
		copyFrom: 1
		to: lastByte