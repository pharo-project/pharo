navigation
moveToLineStart
	self recordSelectionChange: [ 
		selection end moveToLineStart.
		selection collapse
	].
