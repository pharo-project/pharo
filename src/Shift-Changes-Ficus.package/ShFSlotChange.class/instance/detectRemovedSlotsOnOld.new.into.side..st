checking change
detectRemovedSlotsOnOld: oldSlots new: results into: newSlots side: side
	oldSlots
		do: [ :old | 
			newSlots
				detect: [ :new | old name = new name ]
				ifNone: [ results
						add:
							(ShFSlotRemoved new
								slotName: old name;
								slotType: #InstanceVariableSlot;
								slotSide: side;
								yourself) ] ]