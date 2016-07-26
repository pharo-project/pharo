*Compression
isZipArchive
	"Determine if this appears to be a valid Zip archive"
	| sig |
	self binary.
	sig := self next: 4.
	self position: self position - 4. "rewind"
	^ZipArchive validSignatures includes: sig