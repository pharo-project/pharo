accessing
endOfRun: aSymbolOrNil
	endOfRun := aSymbolOrNil.
	
	"Backward compatibility with historical EndOfRun TextConstant handling"
	stops size >= 257 ifTrue: [stops at: 257 put: endOfRun]