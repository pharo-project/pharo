page
updatePageIndex: index
	"Change to the given page index."

	self pageMorph ifNotNil: [:p | self contentMorph removeMorph: p].
	index > 0 ifTrue: [self contentMorph addMorph: (self pages at: index)].
	self pageMorph ifNotNil: [:pm | pm layoutChanged].
	self adoptPaneColor: self paneColor