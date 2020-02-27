Extension { #name : #BorderedMorph }

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> addCornerGrips [
	self addMorphBack: (TopLeftGripMorph new target: self; position: self position).
	self addMorphBack: (TopRightGripMorph new target: self; position: self position).
	self addMorphBack: (BottomLeftGripMorph new target: self; position: self position).
	self addMorphBack: (BottomRightGripMorph new target: self; position: self position)
]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> addPaneSplitters [
	| splitter remaining target targetX sameX minY maxY targetY sameY minX maxX |
	self removePaneSplitters.
	self removeCornerGrips.

	remaining := submorphs reject: [:each | each layoutFrame rightFraction = 1].
	[remaining notEmpty] whileTrue:
		[target := remaining first.
		targetX := target layoutFrame rightFraction.
		sameX := submorphs select: [:each | each layoutFrame rightFraction = targetX].
		minY := (sameX detectMin: [:each | each layoutFrame topFraction]) layoutFrame topFraction.
		maxY := (sameX detectMax: [:each | each layoutFrame bottomFraction]) layoutFrame bottomFraction.
		splitter := ProportionalSplitterMorph new.
		splitter layoutFrame: (
		
		 (targetX @ minY corner: targetX @ maxY) asLayoutFrame 
			leftOffset: target layoutFrame rightOffset;
			topOffset: target layoutFrame topOffset;
			rightOffset: 4 + target layoutFrame rightOffset;
			bottomOffset: target layoutFrame bottomOffset).
			
		self addMorphBack: (splitter position: self position).
		remaining := remaining copyWithoutAll: sameX].

	remaining := submorphs copy reject: [:each | each layoutFrame bottomFraction = 1].
	[remaining notEmpty]
		whileTrue: [target := remaining first.
			targetY := target layoutFrame bottomFraction.
			sameY := submorphs select: [:each | each layoutFrame bottomFraction = targetY].
			minX := (sameY detectMin: [:each | each layoutFrame leftFraction]) layoutFrame leftFraction.
			maxX := (sameY detectMax: [:each | each layoutFrame rightFraction]) layoutFrame rightFraction.
			splitter := ProportionalSplitterMorph new beSplitsTopAndBottom; yourself.
			splitter layoutFrame: (
				(minX @ targetY corner: maxX @ targetY) asLayoutFrame
					leftOffset: target layoutFrame leftOffset;
					topOffset: target layoutFrame bottomOffset;
					rightOffset: target layoutFrame rightOffset;
					bottomOffset: 4 + target layoutFrame bottomOffset ).
			self addMorphBack: (splitter position: self position).
			remaining := remaining copyWithoutAll: sameY].

	self linkSubmorphsToSplitters.
	self splitters do: [:each | each comeToFront].

]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> addPaneVSplitterBetween: leftMorph and: rightMorphs [ 

	| targetX minY maxY splitter |
	targetX := leftMorph layoutFrame rightFraction.
	minY := (rightMorphs detectMin: [:each | each layoutFrame topFraction]) layoutFrame topFraction.
	maxY := (rightMorphs detectMax: [:each | each layoutFrame bottomFraction]) layoutFrame bottomFraction.
	
	splitter := ProportionalSplitterMorph new.
	splitter layoutFrame: ( (targetX @ minY corner: targetX @ maxY) asLayoutFrame 
		leftOffset: leftMorph layoutFrame rightOffset;
		rightOffset: 4+ leftMorph layoutFrame rightOffset;
		topOffset:  leftMorph layoutFrame topOffset;
		bottomOffset: leftMorph layoutFrame bottomOffset).

	self addMorphBack: (splitter position: self position).
]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> linkSubmorphsToSplitters [

	self splitters do:
		[:each |
		each splitsTopAndBottom
			ifTrue:
				[self submorphsDo:
					[:eachMorph |
					(eachMorph ~= each and: [eachMorph layoutFrame bottomFraction = each layoutFrame topFraction]) ifTrue: [each addLeftOrTop: eachMorph].
					(eachMorph ~= each and: [eachMorph layoutFrame topFraction = each layoutFrame bottomFraction]) ifTrue: [each addRightOrBottom: eachMorph]]]
			ifFalse:
				[self submorphsDo:
					[:eachMorph |
					(eachMorph ~= each and: [eachMorph layoutFrame rightFraction = each layoutFrame leftFraction]) ifTrue: [each addLeftOrTop: eachMorph].
					(eachMorph ~= each and: [eachMorph layoutFrame leftFraction = each layoutFrame rightFraction]) ifTrue: [each addRightOrBottom: eachMorph]]]]
]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> removeCornerGrips [

	| corners |
	corners := self submorphsSatisfying: [:each | each isKindOf: CornerGripMorph].
	corners do: [:each | each delete]
]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> removePaneSplitters [
	self splitters do: [:each | each delete]
]

{ #category : #'*Morphic-Widgets-Windows' }
BorderedMorph >> splitters [

	^ self submorphsSatisfying: [:each | each isKindOf: ProportionalSplitterMorph]
]
