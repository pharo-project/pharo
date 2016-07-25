morphic ui
editScript: scriptSymbol

	| script |
	script := workingCopy packageSet perform: scriptSymbol.
	script openLabel: scriptSymbol asString, ' of the Package ', workingCopy package name.