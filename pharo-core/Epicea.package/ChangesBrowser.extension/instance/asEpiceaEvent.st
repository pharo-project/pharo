*Epicea-Triggering
asEpiceaEvent

	self flag: #todo. "This is a workaround for a problem found by RewriteTool"
	(changes allSatisfy: [ :each | each respondsTo: #asEpiceaEvent ])
		ifFalse: [ ^ EpCompositeRefactoring withAll: #() ].

	^ changes size > 1
		ifTrue: [ EpCompositeRefactoring withAll: changes ]
		ifFalse: [ changes anyOne asEpiceaEvent ].