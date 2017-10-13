queries
methodsReadingSlot: aSlot
	^self methods select: [ :method | method readsSlot: aSlot ]