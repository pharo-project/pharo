convenience
uploadEntityFrom: path
	"Set up a ZnStreamingEntity from the binary contents of the file at path.
	The path extension is used to guess the content-type,
	can be overridden using #contentType: if necessary.
	Use the file's length to set the content-length.
	You can then use #put or #post to actually do an upload."
	
	| stream type length |
	stream := ZnFileSystemUtils fileStreamFor: path.
	stream binary.
	type := ZnMimeType forFilenameExtension: (ZnFileSystemUtils extensionFor: path).
	length := ZnFileSystemUtils fileSizeFor: path.
	self entity: (ZnStreamingEntity readFrom: stream usingType: type andLength: length)