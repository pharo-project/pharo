event handling
mapSpecialCharacter: symbol
	^ SDL2SpecialCharacterMapping mapKeySymbol: symbol ifAbsent: [ ^ nil ]