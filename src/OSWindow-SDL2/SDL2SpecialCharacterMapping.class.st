"
This class keeps a maps for key symbols that are represented by Morphic as special characters.
"
Class {
	#name : #SDL2SpecialCharacterMapping,
	#superclass : #Object,
	#classVars : [
		'Mapping'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'class initialization' }
SDL2SpecialCharacterMapping class >> initialize [
	"
	self initialize
	"
	Mapping := Dictionary new.
	{
	 SDLK_RETURN . Character cr.
	 SDLK_BACKSPACE . Character backspace.
	 SDLK_TAB . Character tab.
	 SDLK_HOME . Character home.
	 SDLK_LEFT . Character arrowLeft.
	 SDLK_UP . Character arrowUp.
	 SDLK_RIGHT . Character arrowRight.
	 SDLK_DOWN . Character arrowDown.
	 SDLK_END . Character end.
	 SDLK_INSERT . Character insert.
	 SDLK_PAGEUP . Character pageUp.
	 SDLK_PAGEDOWN . Character pageDown.
	 SDLK_DELETE. Character delete.
	} pairsDo: [ :key :val | Mapping at: key put: val ]
]

{ #category : #'symbol mapping' }
SDL2SpecialCharacterMapping class >> mapKeySymbol: keySym ifAbsent: aBlock [
	^( Mapping at: keySym ifAbsent: [^ aBlock value ]) asCharacter 
]
