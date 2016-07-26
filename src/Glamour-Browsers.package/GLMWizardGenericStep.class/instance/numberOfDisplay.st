accessing
numberOfDisplay
	
	self numberOfDisplayBlock isNil 
		ifFalse: [
			^ self numberOfDisplayBlock moValue: self input
			]
		ifTrue: [
			^ super numberOfDisplay
			].
	