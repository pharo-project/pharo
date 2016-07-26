actions
blockCode: aString notifying: aRequestor
	"Treat the current selection as an expression; evaluate and tally it."
	|  compiledMethod |
	aString ifNil: [^ self].

	blockSource := aString.
	compiledMethod := self compile: aString for: nil in: self doItContext.
	compiledMethod ifNil: [^ self].
	self showResult: ( compiledMethod valueWithReceiver: self arguments: #()).
	self changed: #blockCode.
	self changed: #summaryText.
	self changed: #fullReport.
	self selection: nil.
	self updateList.
	self startState.
	

	