initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@object = nil' with: '``@object isNil';
		replace: '``@object == nil' with: '``@object isNil';
		replace: '``@object ~= nil' with: '``@object notNil';
		replace: '``@object ~~ nil' with: '``@object notNil'