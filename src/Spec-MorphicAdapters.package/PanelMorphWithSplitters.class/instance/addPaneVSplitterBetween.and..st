private
addPaneVSplitterBetween: leftMorphs and: rightMorphs
	"Add a vertical splitter for the given morphs that share a common right fraction.
	If there is a vertical discontinuity apply the splitter to the first contiguous group.
	Answer the morphs to which the splitter was applied."
	
	|targetX fixed bottomFraction topFrame bottomFrame sorted morph leftGroup rightGroup splitter offset|
	leftMorphs ifEmpty: [^self].
	targetX := leftMorphs first layoutFrame rightFraction.
	fixed := leftMorphs select: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction].
		"fixed morphs appear in both top and bottom"
	sorted := ((leftMorphs reject: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction])
		asSortedCollection: [:a :b | a layoutFrame bottomFraction = b layoutFrame bottomFraction
			ifTrue: [a layoutFrame topFraction <= b layoutFrame topFraction]
			ifFalse: [a layoutFrame bottomFraction <= b layoutFrame bottomFraction]]) readStream.
	sorted contents ifEmpty: [^fixed].
	leftGroup := OrderedCollection new.
	bottomFraction := sorted contents first layoutFrame topFraction.
	[sorted atEnd or: [morph := sorted next.
			morph layoutFrame topFraction ~= bottomFraction and: [
				morph layoutFrame bottomFraction ~= bottomFraction]]] whileFalse: [
		leftGroup add: morph.
		bottomFraction := morph layoutFrame bottomFraction].
	topFrame := leftGroup first layoutFrame.
	bottomFrame := leftGroup last layoutFrame.
	rightGroup := (rightMorphs
			reject: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction])
			select: [:m |
		m layoutFrame topFraction
			between: topFrame topFraction
			and: bottomFrame bottomFraction].
	offset := (leftGroup collect: [:m | m layoutFrame rightOffset]) max.
	splitter := ProportionalSplitterMorph new.
	splitter layoutFrame:  ((targetX @ topFrame topFraction corner: targetX @ bottomFrame bottomFraction) asLayoutFrame 
								leftOffset: offset;
								topOffset: topFrame topOffset;
								rightOffset: 4+offset;
								bottomOffset: bottomFrame bottomOffset).
	leftGroup := leftGroup, fixed.
	leftGroup do: [:m | splitter addLeftOrTop: m].
	rightGroup do: [:m | splitter addRightOrBottom: m].
	self addMorphBack: splitter.
	^leftGroup