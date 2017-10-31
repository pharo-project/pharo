tests
testClassWithInheritedSlots
	builder
		name: #SHTestClassWithInheritedSlots;
		superclassName: #ShCBClassWithInstanceVariables;
		slots: #(anSlot anotherSlot).

	result := builder build.

	self validateResult.
	self validateSuperclass: ShCBClassWithInstanceVariables.
	self
		validateSlots:
			{#inheritedSlot => InstanceVariableSlot.
			#anSlot => InstanceVariableSlot.
			#anotherSlot => InstanceVariableSlot}