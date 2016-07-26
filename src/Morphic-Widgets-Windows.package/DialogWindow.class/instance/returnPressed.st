actions
returnPressed
	"Default is to do the default button."

	(self defaultButton ifNil: [^self]) performAction