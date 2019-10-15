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

{ #category : #accessing }
KMShortcutPrinter >> shortcut: aShortcut [

	shortcut := aShortcut
]

{ #category : #private }
KMShortcutPrinter >> shortcutModifiers [
	| modifiers |
	
	modifiers := (shortcut platformModifier asString substrings: '+') collect: #trimBoth.
	"Ensure shift is first"
	((modifiers includes: 'Shift') 
		and: [ modifiers first ~= 'Shift' ])
		ifTrue: [ modifiers := (modifiers copyWithout: 'Shift') copyWithFirst: 'Shift' ].
	
	^ modifiers		
]

{ #category : #visiting }
KMShortcutPrinter >> visitGeneric: aPlatform [
	
	^ String streamContents: [ :stream |
		self shortcutModifiers
			do: [ :each | stream << each ]
			separatedBy: [ stream << '+' ].
		stream << '+'.
		stream << shortcut platformCharacter asUppercase ]
	
]

{ #category : #visiting }
KMShortcutPrinter >> visitMacOS: aPlatform [
	| keyParts symbols |

	keyParts := self shortcutModifiers.
	symbols := keyParts 
		inject: ''
		into: [ :s :each | 
			s, (self class symbolTableAt: each ifAbsent: [ each capitalized ]) ].
		
	^ symbols, (shortcut platformCharacter asString capitalized)
]
