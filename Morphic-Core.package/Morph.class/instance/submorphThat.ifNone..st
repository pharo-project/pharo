submorphs-accessing
submorphThat: block1 ifNone: block2
	^ submorphs detect: [:m | (block1 value: m) == true] ifNone: block2
	