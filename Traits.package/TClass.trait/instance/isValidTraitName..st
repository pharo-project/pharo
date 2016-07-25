private
isValidTraitName: aSymbol
	^(aSymbol isEmptyOrNil
		or: [aSymbol first isLetter not
		or: [aSymbol anySatisfy: [:character | character isAlphaNumeric not]]]) not