private
swap: oneIndex with: otherIndex

	| element |
	element := self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	array swap: oneIndex with: otherIndex
