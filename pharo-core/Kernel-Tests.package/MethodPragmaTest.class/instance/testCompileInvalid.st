testing-compiler
testCompileInvalid
	"Invalid pragmas should properly raise an error."

	self should: [ self compile: '<>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<1>' selector: #zork ] raise: SyntaxErrorNotification.	
	self should: [ self compile: '<#123>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<foo bar>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo 1>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo bar zork>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo bar 1>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<foo: #bar: zork:>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<<1>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<=2>' selector: #zork ] raise: SyntaxErrorNotification.

	self should: [ self compile: '< =1 = >' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '< =1 =2 >' selector: #zork ] raise: SyntaxErrorNotification.