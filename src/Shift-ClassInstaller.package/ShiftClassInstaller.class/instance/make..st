building
make: aBlock
	| newClass |

	aBlock value: builder.

	self lookupOldClass.

	[	
		builder oldClass: oldClass.
		newClass := builder build.

		self installInEnvironment: newClass.

		newClass superclass addSubclass: newClass.
		builder builderEnhancer beforeMigratingClass: builder installer: self.

		self migrateClassTo: newClass.

		builder builderEnhancer afterMigratingClass: builder installer: self.	

		newClass subclasses do: [ :e | self remake: e ].
	] on: ShNoChangesInClass do:[
		"If there are no changes in the building, I am not building or replacing nothing"
		newClass := oldClass.
	].

	self recategorize: newClass to: builder category.
	self comment: newClass.

	self notifyChanges.

	^ newClass