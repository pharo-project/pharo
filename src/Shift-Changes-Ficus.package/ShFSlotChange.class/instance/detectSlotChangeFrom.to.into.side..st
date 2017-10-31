checking change
detectSlotChangeFrom:oldSlots to:newSlots into:results side: side.
	self detectAddedSlotsOnOld: oldSlots new: newSlots into: results side:side.
	self detectRemovedSlotsOnOld: oldSlots new: results into: newSlots side:side.
	self detectSlotTypeChangeOnOld: oldSlots new: newSlots into: results side:side