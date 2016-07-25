installing
installSlotRemoval: slot on: aModification
	0 to: slot size - 1 do: [ :idx |
		(RemovedField new
			originalSlot: slot;
			fieldIndex: idx) installOn: aModification ]