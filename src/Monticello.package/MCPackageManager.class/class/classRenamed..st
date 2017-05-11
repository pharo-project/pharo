system changes
classRenamed: anEvent
	self classModified: anEvent.
	anEvent classAffected extendingPackages
		do: [ :pkg | self managersForPackage: pkg do: [ :mgr | mgr modified: true ] ]