private
buildModificationMapFor: aModification
	aModification slotShift: (ShiftedField new shift: slotShift).
	
	aModification size = 0 
		ifTrue: [ ^ aModification ].
	
	additions do: [ :slot | 
		self installSlotAddition: slot on: aModification].
	
	changes keysAndValuesDo: [ :newSlot :oldSlot | 
		self installSlotChangeFrom: oldSlot to: newSlot on: aModification ].
	
	removals do: [ :slot | 
		self installSlotRemoval: slot on: aModification].
	
	copies keysAndValuesDo: [ :newSlot :oldSlot | 
		self installSlotCopiedFrom: oldSlot to: newSlot on: aModification ].
	
	^ aModification