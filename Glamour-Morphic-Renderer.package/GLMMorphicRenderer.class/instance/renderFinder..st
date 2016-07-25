rendering browsers
renderFinder: aBrowser 
	^ aBrowser hasFixedSizePanes 
		ifTrue: [GLMMorphicFixedPanesFinderRenderer render: aBrowser from: self]
		ifFalse: [GLMMorphicVariablePanesFinderRenderer render: aBrowser from: self]