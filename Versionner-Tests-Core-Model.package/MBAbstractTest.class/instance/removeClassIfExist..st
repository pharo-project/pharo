util
removeClassIfExist: aSymbol
	Smalltalk globals at: aSymbol ifPresent: [ :cls | cls removeFromSystem ]