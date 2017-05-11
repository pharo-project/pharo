menu - filters
filterAllChangesToSelectionBehavior

	| changes |
	changes := self selectedEntryItems collect: [ :each | each entry content ].

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single change, either to a class or a trait' ]. 
	changes anyOne isEpMethodChange ifFalse: [ ^ self inform: 'Should select a method' ]. 

	self addFilter: (EpPluggableFilter isBehaviorNamed: changes anyOne behaviorAffectedName).