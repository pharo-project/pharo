accessing
searchString
	| origString |
	
	origString := super searchString.
	(sourceEntity instVarNames includes: origString) ifTrue: [ 
		^ self modify: origString for: sourceEntity instVarNames ].
	(sourceEntity classVarNames includes: origString) ifTrue: [ 
		^ self modify: origString for: sourceEntity classVarNames ].

	^ origString