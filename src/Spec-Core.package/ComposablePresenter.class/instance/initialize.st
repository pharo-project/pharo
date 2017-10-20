initialization
initialize

	super initialize.
	extentHolder := nil asValueHolder.
	needRebuild := true asValueHolder.
	keyStrokesForNextFocusHolder := { KMNoShortcut new } asValueHolder.
	keyStrokesForPreviousFocusHolder := { KMNoShortcut new } asValueHolder.
	additionalKeyBindings := Dictionary new.
	aboutText := nil asValueHolder.
	windowIcon := nil asValueHolder.
	window := nil asValueHolder.
	askOkToClose := false asValueHolder.
	titleHolder := self class title asValueHolder.
	
	self initializeWidgets.
	self initializePresenter.
	
	keyStrokesForNextFocusHolder whenChangedDo: [ self registerKeyStrokesForNextFor: self widget ].
	keyStrokesForPreviousFocusHolder whenChangedDo: [ self registerKeyStrokesForPreviousFor: self widget ].
	titleHolder whenChangedDo: [ self updateTitle ]