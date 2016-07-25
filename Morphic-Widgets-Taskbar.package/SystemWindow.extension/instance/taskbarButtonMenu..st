*Morphic-Widgets-Taskbar
taskbarButtonMenu: aMenu
	"Answer the menu for the task bar button."

	| menu theme submenu  |
	theme :=  self theme.	
	menu := theme newMenuIn: self for: self.
	
	menu
		addToggle: 'Restore' translated
		target: self
		selector: #restore
		getStateSelector: nil
		enablementSelector: #isNotRestored.
	menu lastItem
		icon: self theme windowMaximizeForm;
		font: theme menuFont.
		
	menu
		addToggle: 'Minimize' translated
		target: self
		selector: #minimize
		getStateSelector: nil
		enablementSelector: #isNotMinimized.
	menu lastItem
		icon: self theme windowMinimizeForm;
		font: theme menuFont.
		
	menu
		addToggle: 'Maximize' translated
		target: self
		selector: #maximize
		getStateSelector: nil
		enablementSelector: #canBeMaximized.
	menu lastItem
		icon: self theme windowMaximizeForm;
		font: theme menuFont.
		
	menu addLine.

	submenu := theme newMenuIn: self for: self.
	menu
		add: 'Close all'
		icon: self theme windowCloseForm
		subMenu: submenu.
	submenu
		addToggle: 'windows to right' translated
		target: self
		selector: #taskbarCloseAllToRight
		getStateSelector: nil
		enablementSelector: true.
	submenu
		addToggle: 'unchanged windows' translated
		target: World
		selector: #closeAllUnchangedWindows
		getStateSelector: nil
		enablementSelector: true.
	submenu
		addToggle: 'close all debuggers' translated
		target: Smalltalk tools debugger
		selector: #closeAllDebuggers
		getStateSelector: nil
		enablementSelector: true.

	menu
		addToggle: 'Close' translated
		target: self
		selector: #closeBoxHit
		getStateSelector: nil
		enablementSelector: #allowedToClose.
	menu lastItem
		icon: self theme windowCloseForm;
		font: theme menuFont.

	^menu