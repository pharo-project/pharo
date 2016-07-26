keymapping
defaultCommandKeymapping
	| cmdMap |
		
	cmdMap := Array new: 256 withAll: #noop:.	"use temp in case of a crash"
	{(Character home -> #cursorHome:).
	(Character end -> #cursorEnd:).
	(Character backspace -> #backspace:).
	(Character pageUp -> #cursorPageUp:).
	(Character newPage -> #cursorPageDown:).
	(Character cr -> #crWithIndent:).
	(Character escape -> #escape:).
	(Character arrowLeft -> #cursorLeft:).
	(Character arrowRight -> #cursorRight:).
	(Character arrowUp -> #cursorUp:).
	(Character arrowDown -> #cursorDown:).
	(Character space -> #selectWord:).
	(Character delete -> #forwardDelete:) } do: [ :assoc | 
		cmdMap at: assoc key asciiValue + 1 put: assoc value ].
	'([{''"<' do: [ :char | cmdMap at: char asciiValue + 1 put: #enclose: ].
	cmdMap at: $, asciiValue + 1 put: #shiftEnclose:.
	'0123456789-=' do: [ :char | cmdMap at: char asciiValue + 1 put: #changeEmphasis: ].
	^ cmdMap