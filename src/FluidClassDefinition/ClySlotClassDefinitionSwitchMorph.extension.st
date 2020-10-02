Extension { #name : #ClySlotClassDefinitionSwitchMorph }

{ #category : #'*FluidClassDefinition' }
ClySlotClassDefinitionSwitchMorph >> toggle [
	Slot showSlotClassDefinition: Slot showSlotClassDefinition not.
	CDFluidClassDefinitionParser setOn: Slot showSlotClassDefinition.
	
	ownerTool update
]
