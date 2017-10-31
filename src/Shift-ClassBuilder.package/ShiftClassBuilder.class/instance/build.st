building
build	
	self tryToFillOldClass.
	self validateSuperclass.
	self compareWithOldClass.

	self layoutDefinition copySlotsIfUsedIn: oldClass.

	self createMetaclass.
	self createClass.

	self createSharedVariables.

	self oldClass ifNotNil: [ 
			self newClass basicCategory: self oldClass basicCategory.
			self copyOrganization.
			builderEnhancer compileMethodsFor: self. ].

	builderEnhancer afterMethodsCompiled: self.
	
	self installSlots.
	
	^ newClass