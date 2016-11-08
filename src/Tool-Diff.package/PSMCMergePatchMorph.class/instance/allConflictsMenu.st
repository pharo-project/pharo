as yet unclassified
allConflictsMenu
	"Answer the all conflicts menu."

	| menu |
	menu := UIManager default newMenuIn: self for: self.
	menu
		addToggle: 'Keep current version of all' translated
		target: self
		selector: #keepAllCurrentVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonKeptConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Shift+Cmd+c'.
	menu
		addToggle: 'Use incoming version of all' translated
		target: self
		selector: #useAllIncomingVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonIncomingConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Shift+Cmd+g'.
	menu
		addToggle: 'Mark all as conflict' translated
		target: self
		selector: #markAllAsConflict
		getStateSelector: nil
		enablementSelector: #hasAnyNonConflictConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancelIcon);
		keyText: 'Shift+Cmd+x'.
	^ menu