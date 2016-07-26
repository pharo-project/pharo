initialization
initialize

	super initialize.
	criticHolder := nil asValueHolder.
	ruleHolder := nil asValueHolder.
	browseModel := self instantiate: ButtonModel.
	transformModel := self instantiate: ButtonModel.
	falsepositiveModel := self instantiate: ButtonModel.
	self setFocusOrder.	
	self setBrowseModel.
	self setTransformModel.
	self setFalsepositiveModel.
	self registerForChanges.