tests
testFullBlocks
	"skip this test in squeak"

"	self blockRuleFor: self currentSelector
		plusSelectors: #(#caller1 #fullBlocks #detectContains #fileBlocks #moveDefinition #caller #assignmentInBlock #equalNotUsed #stringConcatenation #noMoveDefinition #referencesConditionFor: #returnInEnsure)"