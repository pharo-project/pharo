"
A PanelMorphWithSplitters is which add splitters the way SystemWindow do it
"
Class {
	#name : #SpPanelMorphWithSplitters,
	#superclass : #PanelMorph,
	#instVars : [
		'paneMorphs'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'submorphs-add/remove' }
SpPanelMorphWithSplitters >> addMorph: aMorph [

	paneMorphs add: aMorph.
	^ super addMorph: aMorph
]

{ #category : #'submorphs-add/remove' }
SpPanelMorphWithSplitters >> addMorph: aMorph fullFrame: aLayout [

	paneMorphs add: aMorph.
	super addMorph: aMorph fullFrame: aLayout
]

{ #category : #private }
SpPanelMorphWithSplitters >> addPaneHSplitterBetween: topMorphs and: bottomMorphs [
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
]

{ #category : #private }
SpPanelMorphWithSplitters >> addPaneHSplitters [
	"Add the horizontal pane splitters."

	|remaining targetBottom sameBottom sameTop|
	remaining := paneMorphs reject: [:each |
			each layoutFrame bottomFraction = 1 or: [
				each layoutFrame bottomFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetBottom := remaining first layoutFrame bottomFraction.
		sameBottom := remaining select: [:each |
			each layoutFrame bottomFraction = targetBottom].
		sameTop := paneMorphs select: [:each |
			each layoutFrame topFraction = targetBottom].
		remaining := remaining
			copyWithoutAll: (self addPaneHSplitterBetween: sameBottom and: sameTop)]
]

{ #category : #private }
SpPanelMorphWithSplitters >> addPaneSplitters [
	"Add the vertical and horizontal pane splitters."

	self removePaneSplitters.
	self addPaneVSplitters.
	self addPaneHSplitters.
	self linkSplittersToSplitters
]

{ #category : #private }
SpPanelMorphWithSplitters >> addPaneVSplitterBetween: leftMorphs and: rightMorphs [
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
]

{ #category : #private }
SpPanelMorphWithSplitters >> addPaneVSplitters [
	"Add the vertical pane splitters."

	|remaining targetRight sameRight sameLeft |
	remaining := paneMorphs reject: [:each |
			each layoutFrame rightFraction = 1 or: [
				each layoutFrame rightFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetRight := remaining first layoutFrame rightFraction.
		sameRight := remaining select: [:each |
			each layoutFrame rightFraction = targetRight].
		sameLeft := paneMorphs select: [:each |
			each layoutFrame leftFraction = targetRight and: [
				each layoutFrame rightFraction ~= targetRight]].
		remaining := remaining
			copyWithoutAll: (self addPaneVSplitterBetween: sameRight and: sameLeft)]
]

{ #category : #initialization }
SpPanelMorphWithSplitters >> initialize [

	super initialize.

	paneMorphs := OrderedCollection new.
]

{ #category : #'layout-properties' }
SpPanelMorphWithSplitters >> layoutFrame: aFrame [

	self activate.
	super layoutFrame: aFrame
]

{ #category : #private }
SpPanelMorphWithSplitters >> linkSplittersToSplitters [
	"The pane morphs are already linked. Cross link the splitters as appropriate."
	
	self splitters do: [:each |
		each splitsTopAndBottom
			ifTrue: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame bottomFraction = each layoutFrame topFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame topFraction = each layoutFrame bottomFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]]
			ifFalse: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame rightFraction = each layoutFrame leftFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame leftFraction = each layoutFrame rightFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]].
				
		each comeToFront.]
]
