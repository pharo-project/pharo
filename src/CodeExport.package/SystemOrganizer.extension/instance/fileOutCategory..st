*CodeExport
fileOutCategory: category 
	"Store on the file named category (a string) concatenated with '.st' all the 
	classes associated with the category."
	
	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutCategory: category on: internalStream.
	^ CodeExporter writeSourceCodeFrom: internalStream baseName: category isSt: true 