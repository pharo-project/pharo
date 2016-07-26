initialize-release
initSymbols: aClass
	constructor method: method class: aClass literals: method literals.
	constTable := constructor codeConstants.
	instVars := Array new: aClass instSize.
	tempVarCount := method numTemps.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars := self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars := blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps := tempVars ifNil: [(1 to: tempVarCount) collect: [:i| 't', i printString]].
			tempVars := (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i) beMethodArg]