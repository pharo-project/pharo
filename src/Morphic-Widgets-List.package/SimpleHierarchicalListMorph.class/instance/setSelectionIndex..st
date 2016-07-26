keyboard navigation
setSelectionIndex: idx
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index := idx min: scroller submorphs size max: 0.
	theMorph := index = 0 ifTrue: [nil] ifFalse: [scroller submorphs at: index].
	self setSelectedMorph: theMorph.