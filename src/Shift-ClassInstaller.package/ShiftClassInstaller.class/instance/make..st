building
make: aBlock
	| newClass |

	aBlock value: builder.

	self lookupOldClass.

	builder oldClass: oldClass.
	newClass := builder build.

	self installInEnvironment: newClass.

	newClass superclass addSubclass: newClass.
	builder builderEnhancer beforeMigratingClass: builder installer: self.

	self migrateClassTo: newClass.

	builder builderEnhancer afterMigratingClass: builder installer: self.	

	self recategorize: newClass to: builder category.
	self comment: newClass.

	newClass subclasses do: [ :e | self remake: e ].

	self notifyChanges.

	^ newClass