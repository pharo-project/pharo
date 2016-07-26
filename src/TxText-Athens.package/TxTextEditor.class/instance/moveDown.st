navigation
moveDown
	self recordSelectionChange: [ 
		selection end moveDown.
		selection collapse.
	].
