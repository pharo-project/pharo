removing
removeAll
	"Remove all elements from this collection. Preserve the capacity"
	
	| newSelf |
	tally = 0 ifTrue: [^self].
	newSelf := self species newForCapacity: self basicSize.
	self copyFrom: newSelf