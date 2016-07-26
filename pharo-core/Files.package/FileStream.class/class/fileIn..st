file reader services
fileIn: fullName
	"File in the entire contents of the file specified by the name provided"

	| ff fn |
	fullName ifNil: [^ self inform: 'Filename is nil.'].
	fn := fullName asFileReference.
	fn := (Smalltalk hasClassNamed: #GZipReadStream) 
		ifTrue: [(Smalltalk classNamed:  #GZipReadStream) uncompressedFileName: fn fullName]
		ifFalse: [fn fullName].
	ff := self readOnlyFileNamed: fn.
	ff fileIn.
