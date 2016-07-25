writing
addString: string at: path
	| member |
	member := zip addString: string as: path.
	member desiredCompressionMethod: ZipArchive compressionDeflated 
	