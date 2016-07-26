as yet unclassified
stateIcon
	"Answer the state icon to use."

	self wantsClick ifFalse: [^nil].
	^self stateIcons at: (self selectionStates indexOf: self selectionState)