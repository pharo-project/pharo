tests
testVariableLayout
	builder name: #SHClassWithVariableLayout.
	result := builder
		slots: #(anSlot);
		layoutClass: VariableLayout ;
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSlots: {#anSlot => InstanceVariableSlot}.

	self assert: result classLayout class equals: VariableLayout