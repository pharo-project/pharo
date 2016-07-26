resize/collapse
collapseOrExpand
	"Toggle the expand/collapsd state of the receiver.  If expanding, copy the window title back to the name of the expanded morph"

	| aWorld |
	isCollapsed
		ifTrue: 
			[uncollapsedMorph setProperty: #collapsedPosition toValue: self position.
			labelString ifNotNil: [uncollapsedMorph setNameTo: labelString].
			mustNotClose := false.	"We're not closing but expanding"
			self delete.
			(aWorld := self currentWorld) addMorphFront: uncollapsedMorph.
			aWorld startSteppingSubmorphsOf: uncollapsedMorph]
		ifFalse:
			[super collapseOrExpand]