initialization
initialize
	super initialize.
	self anInstVar: 1.
	classBlock := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment