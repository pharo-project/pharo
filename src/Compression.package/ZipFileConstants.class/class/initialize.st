pool initialization
initialize
	"ZipFileConstants initialize"
	
	FaMsdos		:= 0.
	FaUnix 		:= 3.
	DeflatingCompressionNormal		:= 0.
	DeflatingCompressionMaximum	:= 2.
	DeflatingCompressionFast		:= 4.
	DeflatingCompressionSuperFast	:= 6.
	CompressionStored				:= 0.
	CompressionDeflated				:= 8.
	CompressionLevelNone			:= 0.
	CompressionLevelDefault			:= 6.
	IfaTextFile						:= 1.
	IfaBinaryFile					:= 0.
	DataDescriptorLength 				:= 12.

	"Unix permission bits"
	DefaultDirectoryPermissions		:= 8r040755.
	DefaultFilePermissions			:= 8r0100666.
	DirectoryAttrib 					:= 8r040000.
	FileAttrib 						:= 8r0100000.

	"0x02014b50 little-endian i.e. #(16r50 16r4B 16r01 16r02) [1]"
	CentralDirectoryFileHeaderSignature := #[80 75 1 2].
	
	"0x04034b50 little-endian i.e. #(16r50 16r4B 16r03 16r04) [1]"
	LocalFileHeaderSignature := #[80 75 3 4].
	
	"0x06054b50 little-endian i.e. #(16r50 16r4B 16r05 16r06) [1]"
	EndOfCentralDirectorySignature := #[80 75 5 6].
	
	"[1] http://en.wikipedia.org/wiki/Zip_(file_format)#File_headers"