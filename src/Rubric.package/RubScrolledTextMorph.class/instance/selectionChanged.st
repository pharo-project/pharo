accessing-selection
selectionChanged
	self rulers do: [ :r | r selectionChanged ].
	self setModelSelection: self selectionInterval
