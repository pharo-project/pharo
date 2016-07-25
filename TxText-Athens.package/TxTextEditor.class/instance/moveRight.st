navigation
moveRight
	self recordSelectionChange: [ 
		selection end moveRight.
		selection collapse.
	].
