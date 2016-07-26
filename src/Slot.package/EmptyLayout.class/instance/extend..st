extending
extend: someSlots
	^ FixedLayout new
		slotScope: (LayoutEmptyScope instance extend: someSlots)