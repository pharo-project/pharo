testing
allSeen
	
	^ self packages allSatisfy: [ :each | each isSeen ].