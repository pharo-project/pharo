building widgets
rebuildWidget
	| debuggingActions |
	
	debuggingActions := self preDebuggingActions.
	self widgets removeAll.
	self createWidgetsForActions: debuggingActions.
	 
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: (self dynamicLayoutForActions: debuggingActions).
	
	self takeKeyboardFocus. 
