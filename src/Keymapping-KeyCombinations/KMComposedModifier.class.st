"
I represent a composed key modifier. I contain a collection of modifiers that should match with a keyboard event.

Look at my superclass for a more detailed explanaition.
"
Class {
	#name : #KMComposedModifier,
	#superclass : #KMModifier,
	#instVars : [
		'modifiers'
	],
	#category : #Keymapping-KeyCombinations
}

{ #category : #combining }
KMComposedModifier >> + modified [
	^ modified asKeyCombination modifiedBy: self
]

{ #category : #comparing }
KMComposedModifier >> = aShortcut [
	(aShortcut isKindOf: self class)
		ifFalse: [^ false].
	^ (aShortcut modifiers includesAll: modifiers) and: [ modifiers includesAll: aShortcut modifiers ]
]

{ #category : #combining }
KMComposedModifier >> alt [
	modifiers add: KMModifier alt
]

{ #category : #combining }
KMComposedModifier >> command [
	modifiers add: KMModifier command
]

{ #category : #combining }
KMComposedModifier >> control [
	modifiers add: KMModifier ctrl
]

{ #category : #combining }
KMComposedModifier >> ctrl [
	self control
]

{ #category : #comparing }
KMComposedModifier >> hash [
	^ modifiers hash
]

{ #category : #initialization }
KMComposedModifier >> initialize [
	super initialize.
	modifiers := OrderedCollection new.
]

{ #category : #matching }
KMComposedModifier >> matchesEvent: aKeyboardEvent [
	^ modifiers allSatisfy: [:modifier|
		modifier matchesEvent: aKeyboardEvent]
]

{ #category : #combining }
KMComposedModifier >> meta [
	modifiers add: KMModifier meta
]

{ #category : #combining }
KMComposedModifier >> modifiedBy: modifier [
	modifiers add: modifier.
	self updateIdentifier.
]

{ #category : #accessing }
KMComposedModifier >> modifiers [
	^ modifiers
]

{ #category : #printing }
KMComposedModifier >> printOn: aStream [
	modifiers
		do: [ :modifier| modifier printOn: aStream ]
		separatedBy: [ aStream << ' + ' ].
]

{ #category : #combining }
KMComposedModifier >> shift [
	modifiers add: KMModifier shift
]

{ #category : #printing }
KMComposedModifier >> symbol [
	^ String streamContents: [ :stream | 
		self modifiers do: [ :e | 
			stream << e symbol ] ]
]

{ #category : #accessing }
KMComposedModifier >> updateIdentifier [
	identifier := String streamContents: [ :aStream|
			modifiers do: [ :modifier| 
				aStream << modifier identifier]].
	identifier := identifier asSymbol
]
