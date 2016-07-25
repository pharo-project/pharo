private
migrateClasses: classModification.
	
	installer
		migrateClasses: oldClasses asArray
		to: newClasses asArray
		using: classModification instanceModification.