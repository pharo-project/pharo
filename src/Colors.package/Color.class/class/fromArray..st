instance creation
fromArray: colorDef
	colorDef size = 3
			ifTrue: [^self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3)].
	colorDef size = 0
			ifTrue: [^self transparent].
	colorDef size = 4
			ifTrue: [^(self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3)) alpha: (colorDef at: 4)].
	self error: 'Undefined color definition'