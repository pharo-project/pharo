tests
testBadComposedCmdShortcutFails
	^ self should: [ KMModifier ctrl ctrl ] raise: Error