finalization
finalize
	self allSlotsDo: [ :aSlot | aSlot finalize: self ]