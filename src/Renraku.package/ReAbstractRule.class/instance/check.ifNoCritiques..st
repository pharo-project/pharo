running
check: anEntity ifNoCritiques: alternativeBlock
	| critiques |
	
	critiques := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critique | critiques add: critique ] ifNone: alternativeBlock.
	^ critiques