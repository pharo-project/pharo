private
isValidTraitName: aSymbol

	aSymbol ifNil: [ ^ false ].
	^ aSymbol isValidGlobalName.