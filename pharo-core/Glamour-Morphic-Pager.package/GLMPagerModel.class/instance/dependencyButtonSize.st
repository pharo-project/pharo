accessing
dependencyButtonSize
	^ self isDependenciesShown ifTrue: [ self circleButtonSize ] ifFalse: [ 0 ].