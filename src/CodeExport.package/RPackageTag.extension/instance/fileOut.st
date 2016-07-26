*CodeExport
fileOut
	| internalStream |
	
	internalStream := (String new: 1000) writeStream.
	
	SystemOrganization 
		fileOutCategory: self categoryName 
		on: internalStream.
		
	^ FileStream writeSourceCodeFrom: internalStream baseName: self name isSt: true.