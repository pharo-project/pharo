menu commands
yellowButtonActivity: shiftKeyState 
	"Called when the shifted-menu's 'more' item is chosen"
	
	(self getMenu: shiftKeyState)
		ifNotNil: [ :menu|
			menu setInvokingView: self.
			menu invokeModal.
			^ true].
	^ false