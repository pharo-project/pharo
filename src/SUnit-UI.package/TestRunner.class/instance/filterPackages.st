accessing-packages
filterPackages
	| pattern |
	pattern := UIManager default 
		request: 'Pattern to select package:' 
		initialAnswer: '*'.
	pattern isNil ifTrue: [ ^ self ].
	packagesSelected := (packages 
		select: [ :each | pattern match: each ]) asSet.
	self changed: #allSelections; changed: #packageSelected; updateClasses