initialize
wireClasses
	classes
		whenSelectedItemChanged: [ :class | 
			self resetProtocolSelection.
			class 
				ifNil: [ protocols items: #() ] 
				ifNotNil: [ protocols items: (self protocolsOfClass: class).
					         self showClassDefinition ] ]