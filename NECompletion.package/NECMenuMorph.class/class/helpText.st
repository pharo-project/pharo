help text
helpText
	| stream |
	stream := TextStream on: Text new.
	self 
		section: 'character completion'
		on: stream.
	self 
		shortcut: 'works on'
		text: ' [] {} () <> '''' ""'
		on: stream.
	self 
		shortcut: 'usage 1'
		text: 'enter open character - closing character is entered as well'
		on: stream.
	self 
		shortcut: 'usage 2'
		text: 'select some text, enter a smart character and the selected text get surrounded by the opening and closing character.'
		on: stream.
	self 
		section: 'open/close menu'
		on: stream.
	self 
		shortcut: 'ctrl-space or tab'
		text: 'open the completion menu'
		on: stream.
	self 
		shortcut: 'ESC'
		text: 'close menu'
		on: stream.
	self 
		shortcut: 'ctrl-h'
		text: 'open this help'
		on: stream.
	self 
		section: 'menu navigation'
		on: stream.
	self 
		shortcut: 'Arrows up/down'
		text: 'move the selection up and down'
		on: stream.
	self 
		shortcut: 'Page up/down'
		text: 'page up and down'
		on: stream.
	self 
		shortcut: 'Home/End'
		text: 'move to first or last page of the menu'
		on: stream.
	self 
		section: 'show details and browse'
		on: stream.
	self 
		shortcut: 'right arrow (detail closed)'
		text: 'show details about the selected item. This may be the type of the variable, the source of a method or the implementors of the selector.'
		on: stream.
	self 
		shortcut: 'right arrow (detail open)'
		text: 'open a new browser for the selected item.'
		on: stream.
	self 
		shortcut: 'left arrow'
		text: 'close the details'
		on: stream.
	self 
		section: 'changing menu contents'
		on: stream.
	self 
		shortcut: 'ctrl-u'
		text: 'switch to untyped mode in a typed menu'
		on: stream.
	self 
		shortcut: 'ctrl-t'
		text: 'filter out methods of class Object in a typed menu. press again to make the reappear.'
		on: stream.
	self 
		shortcut: 'alphanumeric character'
		text: 'filter the menu to the given input'
		on: stream.
	self 
		shortcut: 'backspace'
		text: 'delete an input character, adjust menu to the new input.'
		on: stream.
	self 
		section: 'inserting completion'
		on: stream.
	self 
		shortcut: 'ctrl-space or tab'
		text: 'close the menu and insert selected completion. if there only one item left in the menu this done automaticly.'
		on: stream.
	^ stream contents