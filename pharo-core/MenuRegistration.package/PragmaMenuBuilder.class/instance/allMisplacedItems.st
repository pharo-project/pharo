registrations handling
allMisplacedItems
	| misplaced |
	self collectMisplacedItemsIn: (misplaced := OrderedCollection new).
	^ misplaced
