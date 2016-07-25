asserting
assertMerge: local with: remote base: ancestor gives: result conflicts: conflictResult
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger
				base: (self snapshotWithElements: local)
				target: (self snapshotWithElements: remote)
				ancestor: (self snapshotWithElements: ancestor).
	merger conflicts do: [:ea | self handleConflict: ea].
	self assert: merger mergedSnapshot definitions hasElements: result.
	self assert: conflicts asSet = conflictResult asSet.