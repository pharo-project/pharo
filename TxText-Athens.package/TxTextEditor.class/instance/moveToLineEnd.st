navigation
moveToLineEnd
	self recordSelectionChange: [ 
		selection end moveToLineEnd.
		selection collapse
	].
