initialization
initializeKeyBindings
	"add keybindings used by table"
	self 
		bindKeyCombination: Character arrowUp shift | Character arrowUp asKeyCombination
		toAction: [ :target :morph :event | self keyStrokeArrowUp: event ].
	self 
		bindKeyCombination: Character arrowDown shift | Character arrowDown asKeyCombination
		toAction: [ :target :morph :event | self keyStrokeArrowDown: event ].
	self 
		bindKeyCombination: Character home asKeyCombination
		toAction: [ self selectRowIndex: 1 ].
	self 
		bindKeyCombination: Character end asKeyCombination
		toAction: [ self selectRowIndex: self numberOfRows ].
	self 
		bindKeyCombination: $a meta  
		toAction: [ self selectAllRowIndexes ]