initialization
initializePresenter
	
	textField acceptBlock: [:text | Smalltalk globals at: text asSymbol
								ifPresent: [:class | methodBrowser messages: class methods ]
								ifAbsent: [ methodBrowser messages: #() ]]