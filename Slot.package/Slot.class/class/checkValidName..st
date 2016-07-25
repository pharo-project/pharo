validating
checkValidName: aSymbol
	aSymbol isPseudovariableName ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ]