accessing
patchRelativeTo: aSnapshot
	^ self workingSnapshot
		patchRelativeToBase: aSnapshot.