private-hacks
addHacks
	(Smalltalk at: #SystemAnnouncer) uniqueInstance suspendAllWhile: [
		super addHacks ]