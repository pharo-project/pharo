private
initDraggedMorph
	draggedMorph ifNotNil: [^self].
	draggedMorph := self passenger asDraggableMorph.
	self addMorphBack: draggedMorph.
	self updateCopyIcon.
	self changed; fullBounds