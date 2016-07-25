initialization
initializeWidgets
	environmentsHolder whenChangedDo: [ self update ].
	enableCustom whenChangedDo: [ self update ].
	enableGlobal whenChangedDo: [ self update ].
	self update.