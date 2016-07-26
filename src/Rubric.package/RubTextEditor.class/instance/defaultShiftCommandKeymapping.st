keymapping
defaultShiftCommandKeymapping
	"Initialize the shift-command-key (or control-key) shortcut table."

	| cmdMap |
	cmdMap := Array new: 256 withAll: #noop:.	"use temp in case of a crash"
	{(Character home -> #cursorHome:).
	(Character end -> #cursorEnd:).
	(Character backspace -> #forwardDelete:).
	(Character pageUp -> #cursorPageUp:).
	(Character newPage -> #cursorPageDown:).
	(Character cr -> #crWithIndent:).
	(Character escape -> #offerMenuFromEsc:).
	(Character arrowLeft -> #cursorLeft:).
	(Character arrowRight -> #cursorRight:).
	(Character arrowUp -> #cursorUp:).
	(Character arrowDown -> #cursorDown:).
	(Character space -> #selectWord:).
	(Character delete -> #forwardDelete:)}
		do: [ :assoc | 
			| char symbol |
			char := assoc key.
			symbol := assoc value.
			cmdMap at: char asciiValue + 1 put: symbol	"plain keys" ].
	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("	"({< and double-quote"	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."
	'9[,''' do: [ :char | cmdMap at: char asciiValue + 1 put: #shiftEnclose: ].
	^ cmdMap