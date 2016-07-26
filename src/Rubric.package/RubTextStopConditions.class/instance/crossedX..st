accessing
crossedX: aSymbolOrNil
	crossedX := aSymbolOrNil.
	
	"Backward compatibility with historical EndOfRun TextConstant handling"
	stops size >= 258 ifTrue: [stops at: 258 put: crossedX]