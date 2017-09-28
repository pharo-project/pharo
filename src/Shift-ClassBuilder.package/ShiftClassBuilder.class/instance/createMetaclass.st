building
createMetaclass
	newMetaclass := self metaclassClass new.
	
	builderEnhancer 
		configureMetaclass: newMetaclass 	
		superclass: (superclass ifNil: [ Class ] ifNotNil: [ superclass class ])
		withLayoutType: FixedLayout
		slots: self layoutDefinition classSlots.
		
	builderEnhancer metaclassCreated: self.