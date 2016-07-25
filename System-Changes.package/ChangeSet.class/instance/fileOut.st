filein/out
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and a unique numeric tag."

	| nameToUse |
	self class promptForDefaultChangeSetDirectoryIfNecessary.
	nameToUse := (self defaultChangeSetDirectory / self name , 'cs') nextVersion basename.
	UIManager default
		showWaitCursorWhile:			
			[ 
			| internalStream |
			internalStream := (String new: 10000) writeStream.
			internalStream
				header;
				timeStamp.
			self fileOutPreambleOn: internalStream.
			self fileOutOn: internalStream.
			self fileOutPostscriptOn: internalStream.
			internalStream trailer.
			FileStream writeSourceCodeFrom: internalStream baseName: (nameToUse copyFrom: 1 to: nameToUse size - 3) isSt: false ]