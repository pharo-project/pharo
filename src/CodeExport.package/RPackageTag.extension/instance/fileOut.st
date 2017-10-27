*CodeExport
fileOut
	| internalStream |
	
	internalStream := (String new: 1000) writeStream.
	
	SystemOrganization 
		fileOutCategory: self categoryName 
		on: internalStream.
		
	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true.