checking change
detectSlotTypeChangeOnOld: oldSlots new: newSlots into: results side: side
	newSlots
		do: [ :new | 
			oldSlots
				detect: [ :old | old name = new name and: [ old slotType ~= new slotType ] ]
				ifFound: [ :old | 
					results
						add:
							(ShFSlotTypeChanged new
								slotName: new name;
								oldSlotType: old slotType;
								slotSide: side;
								newSlotType: new slotType) ] ]