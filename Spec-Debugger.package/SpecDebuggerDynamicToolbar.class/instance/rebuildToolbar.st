building widgets
rebuildToolbar
	| debuggingActions |

	debuggingActions := self debuggingActions.
	self widgets removeAll.
	self createWidgetsForActions: debuggingActions.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: (self dynamicLayoutForActions: debuggingActions)