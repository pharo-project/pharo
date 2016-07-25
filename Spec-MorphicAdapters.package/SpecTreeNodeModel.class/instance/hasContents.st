building
hasContents
	hasContents isBlock
		ifTrue: [ ^ hasContents value ].
	^ self childrenItems notEmpty