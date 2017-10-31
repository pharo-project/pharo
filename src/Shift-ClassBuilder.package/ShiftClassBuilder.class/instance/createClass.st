installing
createClass
	newClass := newMetaclass new.
	newClass setName: self name.

	builderEnhancer 
		configureClass: newClass
		superclass: self superclass
		withLayoutType: self layoutDefinition layoutClass
		slots: self layoutDefinition slots.

	newClass environment: self buildEnvironment environment.
	
	builderEnhancer classCreated: self.