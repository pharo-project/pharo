private
ensureBoundsFrom: fromIndex to: toIndex
	(fromIndex < 1)
		ifTrue: [^self errorSubscriptBounds: fromIndex].
	(toIndex + firstIndex - 1 > lastIndex)
		ifTrue: [^self errorSubscriptBounds: toIndex].