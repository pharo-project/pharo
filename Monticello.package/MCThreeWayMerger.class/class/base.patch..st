instance-creation
base: aSnapshot patch: aPatch
	aPatch isEmpty ifTrue: [MCNoChangesException signal].
	^ self new
		addBaseSnapshot: aSnapshot;
		applyPatch: aPatch;
		yourself
		