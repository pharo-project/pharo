tests
testValueWithinNonLocalReturnFixReal
	"self run: #testValueWithinNonLocalReturnFixReal"
	"The real test for the fix is just as obscure as the original problem"
	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time millisecondClockValue.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time millisecondClockValue - startTime.
		self deny: deltaTime < 90.
	].
