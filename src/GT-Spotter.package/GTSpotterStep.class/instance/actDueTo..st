actions
actDueTo: anEventType
	"Perform an action associated with currently selected item.
	(can be triggered by doubleClick: or keyboard Enter press)"
	
	self
		assert: [ self hasSelected ]
		description: [ 'Spotter can not perform an action due to ', anEventType asString, ' if there is no selected candidate' ].

	self spotter
		historize: self selected;
		notifyActOn: self selectedCandidate for: anEventType.
	self selectedCategory actOn: self selectedCandidate for: self