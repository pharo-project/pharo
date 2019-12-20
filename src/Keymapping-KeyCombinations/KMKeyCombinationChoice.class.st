"
I representing a choice between key combinations.

A keyboard event will match with me when any of my key combinations matches with it.
"
Class {
	#name : #KMKeyCombinationChoice,
	#superclass : #KMKeyCombination,
	#instVars : [
		'shortcuts'
	],
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #'instance creation' }
KMKeyCombinationChoice class >> withShortcuts: someShortcuts [
	^self new
			shortcuts: someShortcuts;
			yourself
]

{ #category : #comparing }
KMKeyCombinationChoice >> = another [

	^ shortcuts anySatisfy: [ :each | another = each ]
]

{ #category : #iterating }
KMKeyCombinationChoice >> collect: aBlock [
	^shortcuts collect: aBlock
]

{ #category : #iterating }
KMKeyCombinationChoice >> combinationsDo: aBlock [
	^shortcuts do: aBlock
]

{ #category : #matching }
KMKeyCombinationChoice >> includes: aShortcut [
	^shortcuts includes: aShortcut
]

{ #category : #matching }
KMKeyCombinationChoice >> matches: anEventBuffer [
	
	^ shortcuts anySatisfy: [ :each | each matches: anEventBuffer ]
]

{ #category : #matching }
KMKeyCombinationChoice >> matchesCompletely: anEventBuffer [
	
	^ shortcuts anySatisfy: [ :each | each matchesCompletely: anEventBuffer ]
]

{ #category : #'accessing platform' }
KMKeyCombinationChoice >> platformCharacter [

	shortcuts do: [ :each | 
		each platformCharacter ifNotNil: [ :char | ^ char ] ].
	^ nil
]

{ #category : #'accessing platform' }
KMKeyCombinationChoice >> platformModifier [

	shortcuts do: [ :each | 
		each platformModifier ifNotNil: [ :mod | ^ mod ] ].
	^ nil
]

{ #category : #printing }
KMKeyCombinationChoice >> prettyPrintOn: aStream [

	(shortcuts select:[ :e | e platform = Smalltalk os platformFamily or: [ e platform = #all ] ])
		do: [ :e | e prettyPrintOn: aStream ]
		separatedBy: [ aStream << ' | ' ]
]

{ #category : #accessing }
KMKeyCombinationChoice >> shortcuts: someShortcuts [
	shortcuts := someShortcuts
]

{ #category : #combining }
KMKeyCombinationChoice >> | aShortcut [
	^KMKeyCombinationChoice withShortcuts: (shortcuts copyWith: aShortcut)
]
