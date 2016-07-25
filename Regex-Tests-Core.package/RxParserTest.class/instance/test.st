tests
test
	"self debug: #test"
	

	self assert: ('\<t\w+' asRegexIgnoringCase
		copy: 'now is the Time'
		translatingMatchesUsing: [:match | match asUppercase]) = 'now is THE TIME'.

	"the regular expression matches words beginning with either an uppercase or a lowercase T"