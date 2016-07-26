action
choose: index
	"Set the given choice and ok."
	| chosen |
	chosen := index > 0 ifTrue: [self labels at: index] ifFalse: [nil].
	chooseBlock value: chosen.
	self close.
	requestor ifNotNil: [requestor takeKeyboardFocus]
