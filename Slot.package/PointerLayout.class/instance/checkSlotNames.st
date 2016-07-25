validation
checkSlotNames
	| slots current |
	slots := slotScope allVisibleSlots.
	[ slots isEmpty ] whileFalse: [
		current := slots removeLast.
		slots do: [ :other |
			other name = current name
				ifTrue: [ 
					DuplicatedSlotName new
						oldSlot: current;
						newSlot: other;
						host: host;
						signal ]]]