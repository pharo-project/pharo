accessing
visibleSlots
	^self slots select: [:slot | slot isVisible]