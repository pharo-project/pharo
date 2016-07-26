typing/selecting keys
querySymbol: aKeyboardEvent
	"Invoked by Ctrl-q to query the Symbol table and display alternate symbols.
	 See comment in completeSymbol:lastOffering: for details."

	self closeTypeIn.
	self editingState closeSelectorChooser.
	self editingState querySymbolFrom: morph.
	^true