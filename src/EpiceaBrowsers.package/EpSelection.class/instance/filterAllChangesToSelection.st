operations
filterAllChangesToSelection

	| changes |
	changes := (self itemsForMenuAction collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single method change' ]. 
	changes anyOne isEpMethodChange ifFalse: [ ^ self inform: 'Should select a method' ]. 

	self filterWith: (EpPluggableFilter isMethod: changes anyOne methodAffected).