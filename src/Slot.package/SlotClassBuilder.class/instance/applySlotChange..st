private
applySlotChange: classModification
	result := self applyAndUpdateSlots: classModification.
	classModification allPropagationsDo: [ :propagation | 
		self applyAndUpdateSlots: propagation ]