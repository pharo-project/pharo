initialization - private
initializePrivateValueHolders

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
	
