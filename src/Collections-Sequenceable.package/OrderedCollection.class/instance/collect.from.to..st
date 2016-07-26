enumerating
collect: aBlock from: fromIndex to: toIndex
	"Override superclass in order to use addLast:, not at:put:."
	| result |
	self ensureBoundsFrom: fromIndex to: toIndex.
	result := self species new: toIndex - fromIndex + 1.
	firstIndex + fromIndex - 1 to: firstIndex + toIndex - 1 do:
		[:index | result addLast: (aBlock value: (array at: index))].
	^ result
