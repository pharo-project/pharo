testing
hasRegistered
	"return true if this package organizer has already registered interest to system events and Monticello changes "
	
	^ self announcer hasSubscriber: self
	
	"|actionSequence|
	self announcer subscriptions ifNil: [^ true].
	actionSequence := self announcer subscriptions values 
								detect: [:each | each anySatisfy: [:anAction | anAction receiver = RPackage organizer]] ifNone: [nil].
	self flag: #cyril.
	^ (actionSequence isNil not) and: [(MCWorkingCopy myDependents includes: self)]"