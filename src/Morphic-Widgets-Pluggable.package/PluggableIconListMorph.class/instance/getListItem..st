model access
getListItem: index
	"get the index-th item in the displayed list"

	getListElementSelector ifNotNil: [
		^self itemMorphFor: (model perform: getListElementSelector with: index) index: index].
	(list notNil and: [list size >= index]) ifTrue: [ ^list at: index ].
	^ self wrapItem: (self getList at: index) index: index