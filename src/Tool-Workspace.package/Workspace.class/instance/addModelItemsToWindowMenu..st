window menu
addModelItemsToWindowMenu: aMenu
	aMenu addLine.
	aMenu add: 'Open...' target: self selector: #openMenuAction.
	aMenu lastItem icon: (self iconNamed: #smallOpenIcon).
	aMenu add: 'Save as...' target: self selector: #saveAsMenuAction.
	aMenu lastItem icon: (self iconNamed: #smallSaveAsIcon).
	aMenu addLine.
	aMenu
		add: 'Encoding'
		subMenu:
			([ :menu | 
			TextConverter allEncodingNames
				do: [ :ea | 
					menu
						addUpdating: #itemForEncoding:
						target: self
						selector: #useEncoding:
						argumentList: {ea} ].
			menu ] value: (UIManager default newMenuIn: self for: self)).
	aMenu addLine.
	aMenu add: 'Inspect variables' target: self selector: #browseBindings.
	aMenu
		add: 'Reset variables'
		target: self
		selector: #initializeBindings.
	aMenu
		add: 'Previous contents...'
		target: self
		selector: #selectPreviousContent.
	aMenu addLine