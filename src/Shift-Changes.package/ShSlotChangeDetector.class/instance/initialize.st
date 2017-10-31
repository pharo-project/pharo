initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition slots asArray ].
	classAccessor := [ :e | e slots asArray].
	comparer := [ :a :b |  self compareSlotCollection: a with: b ]