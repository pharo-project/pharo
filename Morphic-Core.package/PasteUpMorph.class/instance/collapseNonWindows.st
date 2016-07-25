world menu
collapseNonWindows
	self nonWindows reject: [:m | m isSticky] thenDo: [:m | m collapse]