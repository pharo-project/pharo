examples
exampleWindowWithToolbars
	"Open an example window with toolbars."
	"self exampleWindowWithToolbars"
	<script>
	
	| win fileMenu tools dock text holder |
	win := StandardWindow new.
	fileMenu := win newMenu.
	fileMenu addToggle: 'Open' translated target: nil selector: #openFile.	"would be model"
	fileMenu lastItem
		font: win theme menuFont;
		icon: (self iconNamed: #smallOpenIcon);
		keyText: 'Ctrl+O'.
	fileMenu
		addToggle: 'Save' translated
		target: nil
		selector: #saveFile
		getStateSelector: nil
		enablementSelector: nil.
	fileMenu lastItem
		font: win theme menuFont;
		icon: (self iconNamed: #smallSaveIcon);
		keyText: 'Ctrl+S'.
	fileMenu
		addToggle: 'Print...' translated
		target: nil
		selector: #print.
	fileMenu lastItem
		font: win theme menuFont;
		icon: (self iconNamed: #smallPrintIcon);
		keyText: 'Ctrl+P'.
	tools := win
		newToolbar:
			{(win
				newButtonFor: nil
				getState: nil
				action: #openFile
				arguments: nil
				getEnabled: nil
				labelForm: (self iconNamed: #smallOpenIcon)
				help: 'Open file' translated).	"if we had a model it would go here"
			(win
				newButtonFor: nil
				getState: nil
				action: #saveFile
				arguments: nil
				getEnabled: nil
				labelForm: (self iconNamed: #smallSaveIcon)
				help: 'Save file' translated).
			(win
				newButtonFor: nil
				getState: nil
				action: #print
				arguments: nil
				getEnabled: nil
				labelForm: (self iconNamed: #smallPrintIcon)
				help: 'Print' translated)}.
	holder := NewValueHolder new.
	text := win
		newTextEditorFor: holder
		getText: #value
		setText: #value:.
	dock := win newToolDockingBar.
	dock
		add: 'File' translated
			font: win theme menuBarFont
			icon: (self iconNamed: #smallOpenIcon)
			help: 'File operations' translated
			subMenu: fileMenu;
		addMorphBack: tools.
	win
		addMorph: dock
			fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: dock minExtent y);
		addMorph: text
			fullFrame: ((0 @ 0 corner: 1 @ 1) asLayoutFrame topOffset: dock minExtent y).
	win
		themeChanged;
		openInWorld