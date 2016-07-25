model access
getListItem: index
	"get the index-th item in the displayed list"
	| element |
	getListElementSelector ifNotNil: [ ^ self wrapItem: (model perform: getListElementSelector with: index) index: index ].
	list ifNotNil: [ ^ list at: index ].
	
	element := self getList at: index.
	^ self wrapItem: element index: index