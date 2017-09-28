installing
installSlotCopiedFrom: oldSlot to: newSlot on: aModification
	0 to: newSlot size - 1 do: [ :idx |
 		(UnmodifiedField new
			slot: newSlot;
			fieldIndex: idx) installOn: aModification ]