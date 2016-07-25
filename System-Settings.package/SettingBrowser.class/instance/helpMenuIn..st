user interface
helpMenuIn: aWindow
	| helpMenu |
	helpMenu := aWindow newMenu.
	helpMenu
		addToggle: 'About' translated
		target: self
		selector: #about.
	helpMenu lastItem font: aWindow theme menuFont.
	helpMenu
		addToggle: 'Declaring and managing settings' translated
		target: self
		selector: #helpDeclaringAndManagingSettings.
	helpMenu lastItem font: aWindow theme menuFont.
	^ helpMenu
