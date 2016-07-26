widget method
methodsInSystemEnvironmentForCategory: aCategory  in: class
	
	aCategory = self allLabel
		ifTrue: [ ^ class methods asOrderedCollection ].
	
	^ (class methodsInProtocol: aCategory) asOrderedCollection