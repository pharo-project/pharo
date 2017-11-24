"
Similar to KMUntypeableSingleKeyCombination I am used for special keys that don't only rely on their key character.

The difference to KMUntypeableSingleKeyCombination is that I am
typeable but in shortcut descriptions I use a special name for my character. 

For example, a key combnation for the space key is a KMNamedCharKeyCombination and will be printed as
""Shift + SPACE""
rather than
""Shift +  "" 
"
Class {
	#name : #KMNamedCharKeyCombination,
	#superclass : #KMSingleKeyCombination,
	#instVars : [
		'name'
	],
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #testing }
KMNamedCharKeyCombination class >> ifSpecialKey: aCharacter do: aBlock [
	"Lookup if aCharacter needs to be handled by me, if so use one of the instance creation function for creating the corresponding KeyCombination."
	 self specialKeys at: aCharacter ifPresent: [ :symbol |
		aBlock value: (self perform: symbol) ]
]

{ #category : #'instance creation' }
KMNamedCharKeyCombination class >> key: aCharacter name: aString [ 
	^ self new
		key: aCharacter;
		name: aString;
		yourself
]

{ #category : #initialization }
KMNamedCharKeyCombination class >> newSpecialKeys [
"Build a new dictionary that maps untypable characters to symbols for instacation creation methods on this class. For now, only for SPACE Key"
	| dictionary |
	dictionary := Dictionary new.
	#(#space) do: [ :symbol | dictionary at: (Character perform: symbol) put: symbol ].
	^ dictionary
]

{ #category : #'instance creation' }
KMNamedCharKeyCombination class >> space [
	^ self
		key: Character space
		name: 'Space'
		
]

{ #category : #accessing }
KMNamedCharKeyCombination class >> specialKeys [
	^ specialKeys ifNil: [ specialKeys := self newSpecialKeys ]
]

{ #category : #accessing }
KMNamedCharKeyCombination >> name [
	^ name
]

{ #category : #accessing }
KMNamedCharKeyCombination >> name: aSymbol [
	name := aSymbol
]
