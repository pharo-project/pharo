*CodeExport
fileOut  
	"SystemOrganization fileOut"

	| internalStream |
	internalStream := (String new: 30000) writeStream.
	internalStream nextPutAll: 'SystemOrganization changeFromCategorySpecs: #('; cr;
		print: SystemOrganization;  "ends with a cr"
		nextPutAll: ')!'; cr.

	FileStream 
		writeSourceCodeFrom: internalStream 
		baseName: 'SystemOrganization.st' asFileReference nextVersion basenameWithoutExtension 
		isSt: true
