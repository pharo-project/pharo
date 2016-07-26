event-handling
onRevealHints

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self themer spotterThemer hintRevealStyleFor: self diveInButton