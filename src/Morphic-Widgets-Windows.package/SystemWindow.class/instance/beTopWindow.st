activation
beTopWindow
	
	| oldTopWindow |
	
	oldTopWindow := TopWindow.
	TopWindow := self.
	
	oldTopWindow ifNotNil: [ oldTopWindow privateBePassive ].
	
	self announceActivated.