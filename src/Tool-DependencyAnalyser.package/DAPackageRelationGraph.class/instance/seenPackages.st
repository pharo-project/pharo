actions
seenPackages
	^ self packages 
		select: [ :each | each isSeen ]