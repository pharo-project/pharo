initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition classSlots ].
	classAccessor := [ :e | e class slots ].
	comparer := [ :a :b |  self compareSlotCollection: a with: b ]	