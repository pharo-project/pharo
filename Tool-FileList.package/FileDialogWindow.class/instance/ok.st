actions
ok
	"Apply the changes and close."

	self cancelled: false.
	self applyChanges.
	self answer: (self perform: self actionSelector).
	answer ifNil: [
		self cancelled: true.
		^self delete].
	super ok