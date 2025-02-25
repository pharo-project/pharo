"
I represent a sequence of key combinations.

In order to have a match between me and a user's key presses, all user events must match all of my inner shortcuts in the same exact order.

To create a sequence of key combinations use the #, message. Like for example:

```
$a command , $b shift
```
"
Class {
	#name : 'KMKeyCombinationSequence',
	#superclass : 'KMKeyCombination',
	#instVars : [
		'shortcuts'
	],
	#category : 'Keymapping-KeyCombinations',
	#package : 'Keymapping-KeyCombinations'
}

{ #category : 'instance creation' }
KMKeyCombinationSequence class >> first: aShortcut next: anotherShortcut [
	^ (self new)
		first: aShortcut;
		next: anotherShortcut;
		yourself
]

{ #category : 'combining' }
KMKeyCombinationSequence >> + aCharacter [
	| last |
	last := self sequence last.
	self sequence removeLast.
	self addShortcut: last + aCharacter
]

{ #category : 'combining' }
KMKeyCombinationSequence >> , aShortcut [
	self addShortcut: aShortcut asKeyCombination
]

{ #category : 'comparing' }
KMKeyCombinationSequence >> = aShortcut [
	^ super = aShortcut and: [ shortcuts = aShortcut sequence ]
]

{ #category : 'printing' }
KMKeyCombinationSequence >> acceptVisitor: aKMShortcutPrinter [

	^ aKMShortcutPrinter visitCombinationShortcut: self
]

{ #category : 'accessing' }
KMKeyCombinationSequence >> addShortcut: aShortcut [
	self sequence add: aShortcut
]

{ #category : 'matching' }
KMKeyCombinationSequence >> beginsWith: aShortcut [

	^ self sequence first = aShortcut
]

{ #category : 'accessing' }
KMKeyCombinationSequence >> first: aShortcut [
	self addShortcut: aShortcut
]

{ #category : 'comparing' }
KMKeyCombinationSequence >> hash [
	^ shortcuts hash
]

{ #category : 'private' }
KMKeyCombinationSequence >> haveShortcutsSameModifier [

	^ self shortcutModifiers size = 1
]

{ #category : 'matching' }
KMKeyCombinationSequence >> matches: anEventBuffer [
	|maxIndex|
	maxIndex := anEventBuffer size min: shortcuts size.
	1 to: maxIndex do: [:index|
		((shortcuts at: index) = (anEventBuffer at: index) asKeyCombination)
			ifFalse: [ ^ false]].
	^ true
]

{ #category : 'accessing' }
KMKeyCombinationSequence >> next: aShortcut [
	self addShortcut: aShortcut
]

{ #category : 'accessing - platform' }
KMKeyCombinationSequence >> platformModifier [

	self sequence do: [ :each | ^ each platformModifier ].

	^ nil
]

{ #category : 'printing' }
KMKeyCombinationSequence >> prettyPrintOn: aStream [

	shortcuts
		do: [ :e | e prettyPrintOn: aStream ]
		separatedBy: [ aStream << ' + ' ]
]

{ #category : 'printing' }
KMKeyCombinationSequence >> printOn: aStream [

	self haveShortcutsSameModifier
		ifTrue: [ self printSequenceWithSameModifierOn: aStream ]
		ifFalse: [
			shortcuts
				do: [ :shortcut | shortcut printOn: aStream ]
				separatedBy: [ aStream nextPutAll: ' + ' ] ]
]

{ #category : 'private' }
KMKeyCombinationSequence >> printSequenceWithSameModifierOn: aStream [

	| modifier |
	
	modifier := self shortcutModifiers anyOne.
	modifier name
		ifNil: [ aStream nextPutAll: 'Composed' ]
		ifNotNil: [ : modifierName | aStream nextPutAll: modifierName  ].
	aStream nextPutAll: ' + ('.
	shortcuts
		do: [ :s | aStream nextPutAll: s key name ]
		separatedBy: [ aStream nextPutAll: ' , ' ].
	aStream nextPut: $)
]

{ #category : 'accessing' }
KMKeyCombinationSequence >> sequence [
	^ shortcuts ifNil: [ shortcuts := OrderedCollection new ]
]

{ #category : 'private' }
KMKeyCombinationSequence >> shortcutModifiers [
	"Answer a <Set> of the receiver's modifiers, i.e. keys pressed to modify the value of a following key"

	^ shortcuts collect: [ :s | s modifier ] as: Set
]
