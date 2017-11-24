"
I am an abstract class for all single key combinations that correspond to characters that cannot be typed directly.
"
Class {
	#name : #KMUntypeableSingleKeyCombination,
	#superclass : #KMSingleKeyCombination,
	#instVars : [
		'symbol',
		'name'
	],
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> arrowDown [
	^ self
		key: Character arrowDown
		name: 'Down Arrow'
		symbol: '↓'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> arrowLeft [
	^ self
		key: Character arrowLeft
		name: 'Left Arrow'
		symbol: '←'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> arrowRight [
	^ self
		key: Character arrowRight
		name: 'Right Arrow'
		symbol: '→'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> arrowUp [
	^ self
		key: Character arrowUp
		name: 'Up Arrow'
		symbol: '↑'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> backspace [
	^ self
		key: Character backspace
		name: 'Backspace'
		symbol: '⌫'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> delete [
	^ self
		key: Character delete
		name: 'Delete'
		symbol: '⌦'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> end [
	^ self
		key: Character end
		name: 'End'
		symbol: '↘'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> enter [
	^ self
		key: Character enter
		name: 'Enter'
		symbol: '↩'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> escape [
	^ self
		key: Character escape
		name: 'Escape'
		symbol: '⎋'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> home [
	^ self
		key: Character home
		name: 'Home'
		symbol: '↖'
]

{ #category : #testing }
KMUntypeableSingleKeyCombination class >> ifSpecialKey: aCharacter do: aBlock [
	"Lookup if aCharacter needs to be handled by me, if so use one of the instance creation function for creating the corresponding KeyCombination."
	 self specialKeys at: aCharacter ifPresent: [ :symbol |
		aBlock value: (self perform: symbol) ]
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> key: aCharacter name: aString symbol: aVisualString [
	^ self new
		key: aCharacter;
		name: aString;
		symbol: aVisualString;
		yourself
]

{ #category : #initialization }
KMUntypeableSingleKeyCombination class >> newSpecialKeys [
	"Build a new dictionary that maps untypable characters to symbols for instacation creation methods on this class."
	| dictionary |
	dictionary := Dictionary new.
	 #(home enter end backspace tab pageUp pageDown escape arrowLeft arrowRight arrowUp arrowDown delete)
		do: [  :symbol |
			dictionary at: (Character perform: symbol) put: symbol ].
	^ dictionary
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> pageDown [
	^ self
		key: Character pageDown
		name: 'Page Down'
		symbol: '⇟'
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> pageUp [
	^ self
		key: Character pageUp
		name: 'Page Up'
		symbol: '⇞'
]

{ #category : #accessing }
KMUntypeableSingleKeyCombination class >> specialKeys [
	^specialKeys
		ifNil: [ 
			specialKeys := self newSpecialKeys ]
]

{ #category : #'instance creation' }
KMUntypeableSingleKeyCombination class >> tab [
	^ self
		key: Character tab
		name: 'Tab'
		symbol: '⇥'
]

{ #category : #accessing }
KMUntypeableSingleKeyCombination >> name [
	^ name
]

{ #category : #accessing }
KMUntypeableSingleKeyCombination >> name: aString [
	name := aString
]

{ #category : #printing }
KMUntypeableSingleKeyCombination >> symbol [
	"I return the visual symbol for this shortcut.
	For instance for the Arrow Up key I contain a string with an arrow up key symbol: ↑ and now the Character arrowUp."
	^ symbol
]

{ #category : #accessing }
KMUntypeableSingleKeyCombination >> symbol: aString [
	
	symbol := aString
]
