building
createMetaclass
	newMetaclass := self metaclassClass new.
	
	builderEnhancer 
		configureMetaclass: newMetaclass 	
		superclass: self superclass class
		withLayoutType: FixedLayout
		slots: self layoutDefinition classSlots.
		
	builderEnhancer metaclassCreated: self.