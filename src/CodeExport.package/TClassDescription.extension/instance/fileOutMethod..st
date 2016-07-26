*CodeExport
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."
	
	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutMethod: selector on: internalStream.
	FileStream writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '')) isSt: true.