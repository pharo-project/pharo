halos and balloon help
addSimpleHandlesTo: aHaloMorph box: aBox
	| aHandle |

	target isWorldMorph ifTrue: [^ self addSimpleHandlesForWorldHalos].

	self removeAllMorphs.  "remove old handles, if any"
	
	self bounds: (self worldBoundsForMorph: target renderedMorph).  "update my size"
	
	self addHandleAt: (((aBox topLeft + aBox leftCenter) // 2) + self simpleFudgeOffset) color: Color paleBuff icon: #haloMoreHandlesIcon
		on: #mouseDown send: #addFullHandles to: self.

	aHandle := self addGraphicalHandle: #rotateIcon at: aBox bottomLeft on: #mouseDown send: #startRot:with: to: self.
	aHandle on: #mouseMove send: #doRot:with: to: self.

	target shouldFlex
		ifTrue: [(self addGraphicalHandle: #scaleIcon at: aBox bottomRight  on: #mouseDown send: #startScale:with: to: self)
				on: #mouseMove send: #doScale:with: to: self]
		ifFalse: [(self addGraphicalHandle: #scaleIcon at: aBox bottomRight on: #mouseDown send: #startGrow:with: to: self)
				on: #mouseMove send: #doGrow:with: to: self].

	growingOrRotating := false.
	self layoutChanged.
	self changed.
