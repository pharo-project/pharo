accessing - spec fields
bitFlag
	"Per .ZIP File Format Specification Version: 6.3.2:
		2 bytes:
		- Bit 0: If set, indicates that the file is encrypted.

		(For Method 6 - Imploding)
		- Bit 1: If the compression method used was type 6, Imploding, then this bit, if set, indicates an 8K sliding dictionary was used.  If clear, then a 4K sliding dictionary was used.
		- Bit 2: If the compression method used was type 6, Imploding, then this bit, if set, indicates 3 Shannon-Fano trees were used to encode the sliding dictionary output.  If clear, then 2 Shannon-Fano trees were used.

		(For Methods 8 and 9 - Deflating)
		Bit 2  Bit 1
		0      0    Normal (-en) compression option was used.
		0      1    Maximum (-exx/-ex) compression option was used.
		1      0    Fast (-ef) compression option was used.
		1      1    Super Fast (-es) compression option was used.

		(For Method 14 - LZMA)
		Bit 1: If the compression method used was type 14, LZMA, then this bit, if set, indicates an end-of-stream (EOS) marker is used to mark the end of the compressed data stream. If clear, then an EOS marker is not present and the compressed data size must be known to extract.

		Note:  Bits 1 and 2 are undefined if the compression method is any other.

		Bit 3: If this bit is set, the fields crc-32, compressed size and uncompressed size are set to zero in the local header.  The correct values are put in the data descriptor immediately following the compressed data.  (Note: PKZIP version 2.04g for DOS only recognizes this bit for method 8 compression, newer versions of PKZIP recognize this bit for any compression method.)

		Bit 4: Reserved for use with method 8, for enhanced deflating. 

		Bit 5: If this bit is set, this indicates that the file is compressed patched data.  (Note: Requires PKZIP version 2.70 or greater)

		Bit 6: Strong encryption.  If this bit is set, you should set the version needed to extract value to at least 50 and you must also set bit 0.  If AES encryption is used, the version needed to extract value must be at least 51.

		Bit 7: Currently unused.

		Bit 8: Currently unused.

		Bit 9: Currently unused.

		Bit 10: Currently unused.

		Bit 11: Language encoding flag (EFS).  If this bit is set, the filename and comment fields for this file must be encoded using UTF-8. (see APPENDIX D)

		Bit 12: Reserved by PKWARE for enhanced compression.

		Bit 13: Used when encrypting the Central Directory to indicate selected data values in the Local Header are masked to hide their actual values.  See the section describing the Strong Encryption Specification for details.

		Bit 14: Reserved by PKWARE.

		Bit 15: Reserved by PKWARE."
	
	^ bitFlag.