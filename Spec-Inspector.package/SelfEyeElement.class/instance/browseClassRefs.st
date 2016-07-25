action
browseClassRefs

	self hostClass isTrait 
		ifTrue: [self systemNavigation browseAllUsersOfTrait: self hostClass]
		ifFalse: [self systemNavigation browseAllCallsOnClass: self hostClass]