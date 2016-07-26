current
activeClass
	self allSubclassesDo: [:ea | 
		ea isActiveClass ifTrue: [^ ea]].
	^ self