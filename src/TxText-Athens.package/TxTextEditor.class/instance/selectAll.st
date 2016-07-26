selection
selectAll
	self recordSelectionChange: [ selection start moveToStart. selection end moveToEnd ].
