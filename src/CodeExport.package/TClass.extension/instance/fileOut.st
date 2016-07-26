*CodeExport
fileOut
	"Create a file whose name is the name of the receiver with '.st' as the 
	extension, and file a description of the receiver onto it."
	
	| internalStream |
	internalStream := (String new: 100) writeStream.
	internalStream header; timeStamp.

	self hasSharedPools ifTrue: [
		self shouldFileOutPools
			ifTrue: [self fileOutSharedPoolsOn: internalStream]].
	self fileOutOn: internalStream.
	internalStream trailer.

	FileStream writeSourceCodeFrom: internalStream baseName: self name isSt: true