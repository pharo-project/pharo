private
isMatchedItem: anItem
	| itemName |
	itemName := self itemFilterNameFor: anItem.
	^ query allSatisfy: [ :fragment | 
		itemName includesSubstring: fragment caseSensitive: self caseSensitive ]