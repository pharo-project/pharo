extending
extend: someSlots as: type
	| scope fieldIndex  |
	scope := type new: someSlots size.
	fieldIndex := self firstFieldIndex.
	scope parentScope: self.
	
	someSlots withIndexDo: [ :slot :slotIndex | |realSlot|
		realSlot := slot asSlot.
		realSlot scope: scope.
		realSlot isVirtual ifFalse: [realSlot index: fieldIndex].
		fieldIndex := fieldIndex + realSlot size.
		scope at: slotIndex put: realSlot ].
	^ scope