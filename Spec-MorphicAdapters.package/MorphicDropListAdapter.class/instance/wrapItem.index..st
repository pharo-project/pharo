widget API
wrapItem: anItem index: anIndex
	| result |
	result := self model displayForItem: anItem.	
	^ result asString