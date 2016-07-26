private
isMatchedItem: anItem
	^ query match: (self itemFilterNameFor: anItem)