display
initialExtent
	
	| width |
	width := (self listMorph allNodeMorphs detectMax: [:e | e width]) width.
	width := width+ 50.
	
	^width@400