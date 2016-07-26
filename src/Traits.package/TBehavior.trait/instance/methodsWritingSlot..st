queries
methodsWritingSlot: aSlot
	^self methods select: [ :method | method writesSlot: aSlot ]