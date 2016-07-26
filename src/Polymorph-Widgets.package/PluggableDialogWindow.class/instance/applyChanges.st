as yet unclassified
applyChanges
	"Apply the changes."

	super applyChanges.
	self applyChangesSelector ifNotNil: [:s |
		self model perform: s withEnoughArguments: {self}]