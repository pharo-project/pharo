navigation
moveCursorTo: aTextPosition
	self recordSelectionChange: [ 
		selection end: aTextPosition.
		selection collapse.
	].
	