private
string: aString
	"Store this as my string if source files exist."

	SourceFiles changesWriteStreamDo: [ :theFile |
		theFile cr.
		self 
			string: aString 
			onFileNumber: 2
			toFile: theFile ]