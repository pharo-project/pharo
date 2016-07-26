testing
testEmptyCaseStatement
	self flag: 'Opal has the correct behavior but the error is not caught'

	"self setUpForErrorsIn: '^ nil caseOf: { ` At least one case required ->`} '.
	self enumerateAllSelections."