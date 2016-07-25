actions
seenPackagesWithoutExtension
	^ self packages select: [ :each | each isSeen ]