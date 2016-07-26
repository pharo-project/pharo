casing-To be deprecated
caseError
	"DO NOT USE THIS METHOD! It will be removed from Pharo."
	"Report an error from an in-line or explicit case statement."

	self error: 'Case not found (', self printString, '), and no otherwise clause'