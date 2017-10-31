tests
testClassWithClassSlots
	builder
		name: #SHTestClassWithClassSlots;
		slots: #(anSlot);
		classSlots: #(aClassSlot).

	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self
		validateSlots:
			{#anSlot => InstanceVariableSlot}.
			
	self
		validateClassSlots:
			{#aClassSlot => InstanceVariableSlot.}
		superclass: Object