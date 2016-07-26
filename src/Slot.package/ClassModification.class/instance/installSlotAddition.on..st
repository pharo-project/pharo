installing
installSlotAddition: slot on: aModification
	0 to: slot size - 1 do: [ :idx |
		(AddedField new
			newSlot: slot;
			fieldIndex: idx) installOn: aModification  ]