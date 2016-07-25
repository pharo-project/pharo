private - layout
updateExtent
	"private - update the receiver's extent"
	| margin usedHeight |
	self fullBounds.
	self fillsOwner
		ifFalse: [^ self].
	margin := self avoidVisibleBordersAtEdge
				ifTrue: [self borderWidth * 2]
				ifFalse: [0].
	self isHorizontal
		ifTrue: [self width: self owner width + margin].
	self isVertical
		ifTrue: [
			usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top #bottom ).
			self height: self owner height + margin - usedHeight]