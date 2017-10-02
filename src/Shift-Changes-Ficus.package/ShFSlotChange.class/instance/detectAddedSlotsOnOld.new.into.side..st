checking change
detectAddedSlotsOnOld: oldSlots new: newSlots into: results side: side
	newSlots
		do: [ :new | 
			oldSlots
				detect: [ :old | old name = new name ]
				ifNone: [ results
						add:
							(ShFSlotAdded new
								slotName: new name;
								slotType: #InstanceVariableSlot;
								slotSide: side;
								yourself) ] ]