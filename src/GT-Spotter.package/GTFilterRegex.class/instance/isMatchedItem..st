private
isMatchedItem: anItem
	^ self regex matches: (self itemFilterNameFor: anItem)