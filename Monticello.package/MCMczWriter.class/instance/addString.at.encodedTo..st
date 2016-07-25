writing
addString: string at: path encodedTo: encodingName
	| member |
	member := zip addString: (string convertToEncoding: encodingName) as: path.
	member desiredCompressionMethod: ZipArchive compressionDeflated 
	