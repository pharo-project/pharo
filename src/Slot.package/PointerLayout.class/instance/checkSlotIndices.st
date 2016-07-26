validation
checkSlotIndices
	| slots current |
	slots := slotScope flatten select: [:each | each size > 0]. "skip slots that have no index"
	[ slots size > 1 ] whileTrue: [
		current := slots removeFirst.
			self assert: slots first index = (current index + current size) ]