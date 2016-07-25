*System-Object Events
when: anEventSelector evaluate: anAction 

	| actions |
	actions := self actionSequenceForEvent: anEventSelector.
	(actions includes: anAction)
		ifTrue: [^ self].
	self 
		setActionSequence: (actions copyWith: anAction)
		forEvent: anEventSelector