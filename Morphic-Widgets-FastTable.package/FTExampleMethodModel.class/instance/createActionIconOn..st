factory
createActionIconOn: aBrowser
	| actions action |
	actions := AbstractMethodIconAction allSubclasses 
		collect: [ :class | class for: self method in: self ].
	actions sort: [ :a :b | a actionOrder < b actionOrder ].
	action := actions detect: [ :each | each isActionHandled ].
	^ action actionIcon
