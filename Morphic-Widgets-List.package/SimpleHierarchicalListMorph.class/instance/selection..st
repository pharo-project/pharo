selection
selection: item
	"Called from outside to request setting a new selection.
	Assumes scroller submorphs is exactly our list.
	Note: MAY NOT work right if list includes repeated items"

	| i |
	item ifNil: [^self selectionIndex: 0].
	i := scroller submorphs findFirst: [:m | m complexContents == item].
	i > 0 ifTrue: [^self selectionIndex: i].
	i := scroller submorphs findFirst: [:m | m withoutListWrapper = item withoutListWrapper].
	self selectionIndex: i