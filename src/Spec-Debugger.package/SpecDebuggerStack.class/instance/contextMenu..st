initialization
contextMenu: aMenu
	"Create the context menu dynamically."
	
	self flag: 'Spec ignores the shortcuts set in the menu. Use another mechanism to load them'.
	"This method of constructing the menu is used as it allow one to reuse the MenuRegistration class for creating the actual menu morphs."							
	^ aMenu addAllFromPragma: 'specDebuggerStackMenuNew' target: self debugger.