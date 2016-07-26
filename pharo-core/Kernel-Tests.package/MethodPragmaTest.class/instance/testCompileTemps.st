testing-compiler
testCompileTemps
	"Pragmas should be placeable before and after temps."

	self assert: (self compile: '| temps | <foo>' selector: #zork) pragmas notEmpty.
	self assert: (self compile: '<foo> | temps |' selector: #zork) pragmas notEmpty