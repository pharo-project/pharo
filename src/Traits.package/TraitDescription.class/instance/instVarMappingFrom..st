private
instVarMappingFrom: oldClass
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames 
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].