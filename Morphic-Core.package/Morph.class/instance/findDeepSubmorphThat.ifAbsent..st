submorphs-accessing
findDeepSubmorphThat: block1 ifAbsent: block2 
	self
		allMorphsDo: [:m | (block1 value: m)
				== true ifTrue: [^ m]].
	^ block2 value