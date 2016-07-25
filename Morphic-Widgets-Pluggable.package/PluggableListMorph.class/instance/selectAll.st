selection
selectAll

	self isMultipleSelection ifFalse: [ ^ self ].
	1 to: self maximumSelection do: [: i | self listSelectionAt: i put: true ]