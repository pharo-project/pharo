private
isMatchedItem: anItem
	^ self matcher 
		ifNotNil: [ self matcher value: anItem value: self context ]
		ifNil: [ (self itemFilterNameFor: anItem) includesSubstring: query caseSensitive: caseSensitive ]
		