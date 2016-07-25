dropping/grabbing
justDroppedInto: aMorph event: anEvent 
	| ownerBounds leftRegion droppedPosition rightRegion topRegion bottomRegion |
	super justDroppedInto: aMorph event: anEvent.
	
	self owner isNil ifTrue: [^ self].
	ownerBounds := aMorph bounds.
	topRegion := ownerBounds bottom: ownerBounds top + (ownerBounds height // 5).
	bottomRegion := ownerBounds top: ownerBounds bottom - (ownerBounds height // 5).

	leftRegion := ownerBounds right: ownerBounds left + (ownerBounds width // 5).
	leftRegion := leftRegion top: topRegion bottom.
	leftRegion := leftRegion bottom: bottomRegion top.
	rightRegion := ownerBounds left: ownerBounds right - (ownerBounds width // 5).
	rightRegion := rightRegion top: topRegion bottom.
	rightRegion := rightRegion bottom: bottomRegion top.

	droppedPosition := anEvent position.
	(topRegion containsPoint: droppedPosition) ifTrue: [
			^ self adhereToTop].
	(bottomRegion containsPoint: droppedPosition) ifTrue: [
			^ self adhereToBottom].
	(leftRegion containsPoint: droppedPosition) ifTrue: [
			^ self adhereToLeft].
	(rightRegion containsPoint: droppedPosition) ifTrue: [
			^ self adhereToRight].
	self beFloating