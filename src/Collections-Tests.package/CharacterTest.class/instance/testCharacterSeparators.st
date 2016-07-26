tests - various
testCharacterSeparators
	"Regression test"

	| result |
	result := '/' , Character separators.
	self assert: result size = (Character separators size + 1)