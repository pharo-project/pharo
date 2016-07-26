selection
selectionIndex: idx
	"Called internally to select the index-th item."
	| theMorph range index |
	idx ifNil: [^ self].
	index := idx min: scroller submorphs size max: 0.
	(theMorph := index = 0 ifTrue: [nil] ifFalse: [scroller submorphs at: index])
		ifNotNil:
		[((theMorph bounds top - scroller offset y) >= 0
			and: [(theMorph bounds bottom - scroller offset y) <= self innerBounds height]) ifFalse:
			["Scroll into view -- should be elsewhere"
			range := self vTotalScrollRange.
			scrollBar value: (range > 0
				ifTrue: [((index-1 * theMorph height) / self vTotalScrollRange)
									truncateTo: scrollBar scrollDelta]
				ifFalse: [0]).
			scroller offset: -3 @ (range * scrollBar value)]].
	self selectedMorph: theMorph