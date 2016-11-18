private
migrateClasses: classModification
	| dependantClasses |
	
	installer
		migrateClasses: oldClasses asArray
		to: newClasses asArray
		using: classModification instanceModification.

	"we announce events only about dependant classes because here we are not finished current class modification (result variable)"
	dependantClasses := newClasses copyWithoutAll: { result instanceSide. result classSide }.
	dependantClasses do: [ :each | 
		SystemAnnouncer uniqueInstance classModificationAppliedTo: each  ]