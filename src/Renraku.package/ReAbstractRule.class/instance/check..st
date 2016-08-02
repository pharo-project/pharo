running
check: anEntity

	| critiques |
	
	critiques := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critique | critiques add: critique ].
	^ critiques