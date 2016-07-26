accessing
color
	^ complexContents color ifNil: [self index ifNotNil: [container rowColors at: ((self index \\ 2) + 1)]]