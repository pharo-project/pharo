pool initialization
initialize
	"GZipConstants initialize"
	GZipMagic := 16r8B1F.		"GZIP magic number"
	GZipDeflated := 8.			"Compression method"

	GZipAsciiFlag := 16r01.		"Contents is ASCII"
	GZipContinueFlag := 16r02.	"Part of a multi-part archive"
	GZipExtraField := 16r04.		"Archive has extra fields"
	GZipNameFlag := 16r08.		"Archive has original file name"
	GZipCommentFlag := 16r10.	"Archive has comment"
	GZipEncryptFlag := 16r20.	"Archive is encrypted"
	GZipReservedFlags := 16rC0.	"Reserved" 