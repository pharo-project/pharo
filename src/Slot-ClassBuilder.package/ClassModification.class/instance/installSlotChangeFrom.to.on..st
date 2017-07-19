installing
installSlotChangeFrom: oldSlot to: newSlot on: aModification
	0 to: newSlot size - 1 do: [ :idx |
 		(ModifiedField new
			newSlot: newSlot;
			originalSlot: oldSlot;
			fieldIndex: idx) installOn: aModification ]