building
make: aBlock
	| newClass |
		
	builder := ShiftClassBuilder new.
	builder buildEnvironment: ShSmalltalkGlobalsEnvironment new.	
	
	aBlock value: builder.

	oldClass ifNil:[
		oldClass := builder environment at: (builder name) ifAbsent:[nil]].
	builder oldClass: oldClass.

	newClass := builder build.

	builder environment at: (builder name) put: newClass.
	self updateBindings: (builder environment associationAt: (builder name)) of:newClass.
	
	newClass superclass addSubclass: newClass.
	
	builder builderEnhancer beforeMigratingClass: builder installer: self.
		
	self migrateClassTo:newClass.
	
	self recategorize: newClass to: builder category.
	self comment: newClass.
	
	newClass subclasses do:[:e | self remake: e ].
	
	builder notifyChanges.	
		
	^ newClass.