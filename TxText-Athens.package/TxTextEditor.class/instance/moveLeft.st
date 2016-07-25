navigation
moveLeft
	(selection end isAtStart and: [ selection end span prev isAtStart ])
		ifTrue: [ ^ self ].
	self recordSelectionChange: [ 
		selection end moveLeft.
		selection collapse
	].