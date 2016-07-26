error handing
checkForError
	| msg |
	msg := self getErrorMessage.
	( msg ~= SDL2 and: msg ~= '' ) ifTrue:[SDL2Error signal: msg]