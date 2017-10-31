initialization
initialize
	super initialize.
	self
		replace: '``@object = nil' with: '``@object isNil';
		replace: '``@object == nil' with: '``@object isNil';
		replace: '``@object ~= nil' with: '``@object notNil';
		replace: '``@object ~~ nil' with: '``@object notNil'