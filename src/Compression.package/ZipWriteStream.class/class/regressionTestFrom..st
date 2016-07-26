regression test
regressionTestFrom: fd
	"ZipWriteStream regressionTestFrom: FileSystem disk workingDirectory"
	"ZipWriteStream regressionTestFrom: (FileSystem disk root)"
	| tempFile stats |
	Transcript clear.
	stats := Dictionary new.
	tempFile := FileSystem disk workingDirectory / '$$sqcompress$$'.
	tempFile delete.
	self regressionTestFrom: fd using: tempFile stats: stats.