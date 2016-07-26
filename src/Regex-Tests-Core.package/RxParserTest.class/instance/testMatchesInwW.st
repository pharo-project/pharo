tests
testMatchesInwW
	"self debug: #testMatchesInwW"
	
	"1. Backslash escapes similar to those in Perl are allowed in patterns:
	\w	any word constituent character (equivalent to [a-zA-Z0-9:=])
	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9:=]"

	self assert: ('\w+' asRegex matchesIn: 'now is the time') asArray = #('now' 'is' 'the' 'time').
	self assert: ('\W+' asRegex matchesIn: 'now is the time') asArray = #(' ' ' ' ' ').
	
	"why do we get that"
	self assert: ('\w' asRegex matchesIn: 'now') asArray = #('n' 'o' 'w').