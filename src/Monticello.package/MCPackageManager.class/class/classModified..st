system changes
classModified: anEvent
	self 
		managersForClass: anEvent classAffected 
		do:[ :mgr | mgr modified: true ].