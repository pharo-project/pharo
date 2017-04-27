visitor
visitBehaviorNameChange: aChange

	self 
		behaviorNamed: aChange oldName
		ifPresent: [ :behavior | 
			^ { EpBehaviorNameChange 
					oldName: aChange oldName
					newName: aChange newName
					class: behavior } ].

	^ #()