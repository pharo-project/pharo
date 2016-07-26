tests
testAnalogousCodeTo
	"create a fake traitSource association property"
	| state |
	state := AdditionalMethodState new: 1.
	state 
		basicAt: 1
		put: #traitSource -> (TBehavior methodNamed: #>>).
		
	self 
		shouldnt: [ state analogousCodeTo: state ]
		raise: MessageNotUnderstood