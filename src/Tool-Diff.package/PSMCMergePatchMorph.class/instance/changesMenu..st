as yet unclassified
changesMenu: m
	"Answer the changes menu."

	| menu |
	menu := super changesMenu: m.
	menu addLine.
	menu
		addToggle: 'Apply this change' translated
		target: self
		selector: #toggleApplySelectedChange
		getStateSelector: #selectionIsRemoteChosen
		enablementSelector: #selectionIsNotNil.
	menu
		addToggle: 'Keep current version' translated
		target: self
		selector: #keepCurrentVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Cmd+c'.
	menu
		addToggle: 'Use incoming version' translated
		target: self
		selector: #useIncomingVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Cmd+g'.
	menu
		addToggle: 'Mark as conflict' translated
		target: self
		selector: #markAsConflict
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancelIcon);
		keyText: 'Cmd+x'.
	menu
		addToggle: 'All conflicts' translated
		target: self
		selector: nil
		getStateSelector: nil
		enablementSelector: #hasConflicts.
	menu lastItem
		font: self theme menuFont;
		subMenu: self allConflictsMenu.
	menu addLine.
	menu
		addToggle: 'Select next conflict' translated
		target: self
		selector: #selectNextConflict
		getStateSelector: nil
		enablementSelector: #notAllConflictsResolved.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallRightFlushIcon);
		keyText: 'Cmd+f'.
	^ menu