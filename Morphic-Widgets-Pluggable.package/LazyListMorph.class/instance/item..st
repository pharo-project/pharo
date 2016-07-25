list access
item: index
	"return the index-th item, using the 'listItems' cache"
	| newItem itemWidth |
	(index between: 1 and: listItems size)
		ifFalse: [ "there should have been an update, but there wasn't!"  ^self getListItem: index].
	(listItems at: index) ifNil: [ 
		newItem := self getListItem: index.
		"Update the width cache."
		maxWidth ifNotNil:[
			itemWidth := newItem widthToDisplayInList: self.
			itemWidth > maxWidth ifTrue:[
				maxWidth := itemWidth.
				self adjustWidth.
			]].
		listItems at: index put: newItem ].
	^listItems at: index