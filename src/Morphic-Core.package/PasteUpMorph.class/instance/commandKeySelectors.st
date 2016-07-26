world menu
commandKeySelectors
	"Answer my command-key table"

	| aDict |
	aDict := self valueOfProperty: #commandKeySelectors ifAbsentPut: [self initializeDesktopCommandKeySelectors].
	^ aDict