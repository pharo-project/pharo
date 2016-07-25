accessing
stepChildren
	"Ensure we load step children as appropriate if necessary."

	(stepChildren anySatisfy: #isNil)
		ifTrue: [ self loadAncestorsAndStepChildren ].
	(stepChildren anySatisfy: #isNil)
		ifTrue: [ ^ #() ].
	^ stepChildren