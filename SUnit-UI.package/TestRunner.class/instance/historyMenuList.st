history saving
historyMenuList
	^ {'** save current result **'}, (self previousRun collect: [:ts | ts printString])