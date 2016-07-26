tests
testMultiPackageMerge3
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger new.
	merger addBaseSnapshot: (self snapshotWithElements: #(a1 b1)).
	merger applyPatch: ((self snapshotWithElements: #(a1 b1)) patchRelativeToBase: (self snapshotWithElements: #(b1))).
	merger applyPatch: ((self snapshotWithElements: #()) patchRelativeToBase: (self snapshotWithElements: #(a1))).
	merger conflicts do: [:ea | self handleConflict: ea].
	self assert: merger mergedSnapshot definitions hasElements: #(a1 b1).
	self assert: conflicts isEmpty