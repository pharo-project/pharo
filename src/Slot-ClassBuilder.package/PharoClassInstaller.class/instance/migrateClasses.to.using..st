migrating
migrateClasses: old to: new using: anInstanceModification
	instanceModification := anInstanceModification.
	old ifEmpty:  [ ^ self ].
	[
		1 to: old size do: [ :index |
			self updateClass: (old at: index) to: (new at: index)].
		old elementsForwardIdentityTo: new.
		" Garbage collect away the zombie instances left behind in garbage memory in #updateInstancesFrom: "
		" If we don't clean up this garbage, a second update would revive them with a wrong layout! "
		" (newClass rather than oldClass, since they are now both newClass) "
		Smalltalk garbageCollect.
	] valueUnpreemptively