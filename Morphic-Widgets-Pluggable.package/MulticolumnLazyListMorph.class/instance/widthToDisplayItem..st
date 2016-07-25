scroll range
widthToDisplayItem: item
	"This class will be removed soon, so this method will disappear"
	| widths |
	
	widths := item collect: [ :each | each widthToDisplayInList: self ].
	^widths sum + ((listSource gapSize + 4) * (widths size - 1))   "add in space between the columns"
