protocol
changesMenu: m
	"Answer the changes menu."

	| menu |
	menu := self newMenu
		addTitle: 'Changes'
		icon: (self iconNamed: #smallCopyIcon).
	menu
		addToggle: 'Browse class...' translated
		target: self
		selector: #browseClass
		getStateSelector: nil
		enablementSelector: #selectionHasActualClass.
	menu lastItem
		font: self theme menuFont;
		icon: Smalltalk tools browser taskbarIcon;
		keyText: 'Cmd+b'.
	menu addLine.
	menu
		addToggle: 'Versions...' translated
		target: self
		selector: #browseVersions
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallJustifiedIcon);
		keyText: 'Cmd+v'.
	menu
		addToggle: 'Senders...' translated
		target: self
		selector: #browseSenders
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Cmd+n'.
	menu
		addToggle: 'Implementors...' translated
		target: self
		selector: #browseImplementors
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallDoItIcon);
		keyText: 'Cmd+m'.
	menu
		addToggle: 'Install incoming version' translated
		target: self
		selector: #loadMethodSelection
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallUpdateIcon).
	menu
		addToggle: 'Revert change' translated
		target: self
		selector: #revertSelectedChange
		getStateSelector: nil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancelIcon).
	menu
		add: 'Expand All' translated
		target: self model
		selector: #expandAll.
	menu
		add: 'Collapse All' translated
		target: self model
		selector: #collapseAll.
	^ menu