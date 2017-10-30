private
addPaneHSplitterBetween: topMorphs and: bottomMorphs
	"Add a horizontal splitter for the given morphs that share a common bottom fraction.
	If there is a horizontal discontinuity apply the splitter to the first contiguous group.
	Answer the morphs to which the splitter was applied."
	
	|targetY fixed rightFraction leftFrame rightFrame sorted morph topGroup bottomGroup splitter offset|
	topMorphs ifEmpty: [^self].
	targetY := topMorphs first layoutFrame bottomFraction.
	fixed := topMorphs select: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction].
		"fixed morphs appear in both top and bottom"
	sorted := ((topMorphs reject: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction])
		asSortedCollection: [:a :b | a layoutFrame rightFraction = b layoutFrame rightFraction
			ifTrue: [a layoutFrame leftFraction <= b layoutFrame leftFraction]
			ifFalse: [a layoutFrame rightFraction <= b layoutFrame rightFraction]]) readStream.
	sorted contents ifEmpty: [^fixed].
	topGroup := OrderedCollection new.
	rightFraction := sorted contents first layoutFrame leftFraction.
	[sorted atEnd or: [morph := sorted next.
			(morph layoutFrame leftFraction ~= rightFraction and: [
				morph layoutFrame rightFraction ~= rightFraction])]] whileFalse: [
		topGroup add: morph.
		rightFraction := morph layoutFrame rightFraction].
	leftFrame := topGroup first layoutFrame.
	rightFrame := topGroup last layoutFrame.
	bottomGroup := (bottomMorphs 
			reject: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction])
			select: [:m |
		(m layoutFrame leftFraction
			between: leftFrame leftFraction
			and: rightFrame rightFraction) or: [
		m layoutFrame rightFraction
			between: leftFrame leftFraction
			and: rightFrame rightFraction]].
	offset := (topGroup collect: [:m | m layoutFrame bottomOffset]) max.
	splitter := ProportionalSplitterMorph new beSplitsTopAndBottom.
	splitter layoutFrame: ( (leftFrame leftFraction @ targetY corner: rightFrame rightFraction @ targetY) asLayoutFrame 
								leftOffset: leftFrame leftOffset;
								rightOffset: rightFrame rightOffset;
								topOffset: offset;
								bottomOffset: 4+offset ).
	topGroup := topGroup, fixed.
	topGroup do: [:m | splitter addLeftOrTop: m].
	bottomGroup do: [:m | splitter addRightOrBottom: m].
	self addMorphBack: splitter.
	^topGroup