navigation
moveUp
	self recordSelectionChange: [ 
		selection end moveUp.
		selection collapse.
	].
