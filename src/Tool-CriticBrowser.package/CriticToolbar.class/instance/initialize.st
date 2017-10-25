initialization
initialize

	super initialize.
	criticHolder := nil asValueHolder.
	ruleHolder := nil asValueHolder.
	browseModel := self instantiate: ButtonPresenter.
	transformModel := self instantiate: ButtonPresenter.
	falsepositiveModel := self instantiate: ButtonPresenter.
	self setFocusOrder.	
	self setBrowseModel.
	self setTransformModel.
	self setFalsepositiveModel.
	self registerForChanges.