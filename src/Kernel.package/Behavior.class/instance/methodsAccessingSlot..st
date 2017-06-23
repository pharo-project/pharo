queries
methodsAccessingSlot: aSlot
	^self methods select: [ :method | method accessesSlot: aSlot ]