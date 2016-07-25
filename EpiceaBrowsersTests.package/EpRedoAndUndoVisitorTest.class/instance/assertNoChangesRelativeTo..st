asserting
assertNoChangesRelativeTo: aSnapshot
	self assert: (EpMiniMonticelloResource current patchRelativeTo: aSnapshot) isEmpty.