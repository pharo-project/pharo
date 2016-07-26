private
replacing: aUIManager

	" save the aUIManager to restore it later, when we become interactive again "
	
	uiManager := aUIManager.
	
	self beDefault.