extending
extendVariable: someSlots
	^ VariableLayout new
		slotScope: (LayoutEmptyScope extend: someSlots)