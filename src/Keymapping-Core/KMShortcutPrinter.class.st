"
I'm an util to convert a shortcut into the representation used in a platform (they change, as the shortcut themselves are different sometimes)

"
Class {
	#name : #KMShortcutPrinter,
	#superclass : #OSPlatformVisitor,
	#instVars : [
		'shortcut'
	],
	#classVars : [
		'SymbolTable'
	],
	#category : #'Keymapping-Core-Base'
}

{ #category : #private }
KMShortcutPrinter class >> createSymbolTable [

	^ Dictionary newFromPairs: { 
		#Cmd. 		(Character value: 16r2318) asString.
		#Meta. 	(Character value: 16r2318) asString.
		#Alt. 		(Character value: 16r2325) asString. 
		#Ctrl.  	(Character value: 16r2303) asString. 
		#Shift. 	(Character value: 16r21E7) asString.
		#Enter. 	(Character value: 16r23ce) asString }
]

{ #category : #private }
KMShortcutPrinter class >> symbolTable [

	^ SymbolTable ifNil: [ SymbolTable := self createSymbolTable ]
]

{ #category : #private }
KMShortcutPrinter class >> symbolTableAt: anObject ifAbsent: aBlock [

	^ self symbolTable 
		at: anObject 
		ifAbsent: aBlock
]

{ #category : #'instance creation' }
KMShortcutPrinter class >> toString: aShortcut [

	^ self new
		shortcut: aShortcut;
		visit
]

{ #category : #private }
KMShortcutPrinter >> genericShortcutFor: aShortcut [

	| char |
	char := self genericSpecialCharacter: aShortcut platformCharacter.	
	^ String streamContents: [ :stream |
		(self shortcutModifiersOf: aShortcut)
			do: [ :each | stream << each ]
			separatedBy: [ stream << '+' ].
		stream << '+'.
		stream << char ]
	
]

{ #category : #visiting }
KMShortcutPrinter >> genericSpecialCharacter: char [ 

	char = KeyboardKey enter ifTrue: [ ^ 'Enter' ].
	char = (KeyboardKey fromCharacter: Character space) ifTrue: [ ^ 'Space' ].
	^ char	 name
]

{ #category : #visiting }
KMShortcutPrinter >> macSpecialCharacter: char [ 

	char = KeyboardKey enter ifTrue: [ ^ self class symbolTableAt: #Enter ifAbsent: [ 'Enter' ] ].	
	(char = (KeyboardKey fromCharacter: Character space))
		ifTrue: [ ^ self class symbolTableAt: #Space ifAbsent: [ 'Space' ] ].	
	^ char name asUppercase
]

{ #category : #private }
KMShortcutPrinter >> osxShortcutFor: aShortcut [

	| keyParts symbols char |

	keyParts := self shortcutModifiersOf: aShortcut.
	symbols := keyParts 
		inject: ''
		into: [ :s :each | 
			s, (self class symbolTableAt: each ifAbsent: [ each capitalized ]) ].
	
	char := self macSpecialCharacter: aShortcut platformCharacter.
	^ symbols, char asString
]

{ #category : #accessing }
KMShortcutPrinter >> shortcut: aShortcut [

	shortcut := aShortcut
]

{ #category : #private }
KMShortcutPrinter >> shortcutModifiersOf: aShortcut [
	| modifiers |
	
	modifiers := (aShortcut platformModifier asString substrings: '+') collect: [:each | each trimBoth].
	"Ensure shift is first"
	((modifiers includes: 'Shift') 
		and: [ modifiers first ~= 'Shift' ])
		ifTrue: [ modifiers := (modifiers copyWithout: 'Shift') copyWithFirst: 'Shift' ].
	
	^ modifiers		
]

{ #category : #visiting }
KMShortcutPrinter >> visitCombinationShortcutOnGeneric: aShortcut [

	^ (shortcut sequence collect: [ :e | self genericShortcutFor: e ]) joinUsing: ','
]

{ #category : #visiting }
KMShortcutPrinter >> visitCombinationShortcutOnOSX: aShortcut [

	^ (shortcut sequence collect: [ :e | self osxShortcutFor: e ]) joinUsing: ','
]

{ #category : #visiting }
KMShortcutPrinter >> visitGeneric: aPlatform [
	
	^ shortcut acceptVisitorForGenericPlatform: self
]

{ #category : #visiting }
KMShortcutPrinter >> visitMacOS: aPlatform [
	
	^ shortcut acceptVisitorForOSXPlatform: self
]

{ #category : #visiting }
KMShortcutPrinter >> visitSingleShortcutOnGeneric: aShortcut [

	^ self genericShortcutFor: shortcut
]

{ #category : #visiting }
KMShortcutPrinter >> visitSingleShortcutOnOSX: aShortcut [

	^ self osxShortcutFor: shortcut
]
