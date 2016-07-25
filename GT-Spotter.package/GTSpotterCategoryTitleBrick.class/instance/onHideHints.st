event-handling
onHideHints

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self themer spotterThemer hintHideStyleFor: self diveInButton