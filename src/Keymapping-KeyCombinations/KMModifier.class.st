"
I'm an abstract class representing a key modifier. A modifier is a key that is pressed to modify the value of a following key. For example, valid modifiers are Alt, Ctrl, Cmd (only for macosx), Shift. Additionally, I could be a composed modifier, combining several of the named above. For example, I could have a value of Alt + Ctrl.
"
Class {
	#name : #KMModifier,
	#superclass : #KMKeyCombination,
	#instVars : [
		'identifier',
		'name'
	],
	#category : #Keymapping-KeyCombinations
}

{ #category : #'instance creation' }
KMModifier class >> alt [
	^ KMAltModifier new
]

{ #category : #'instance creation' }
KMModifier class >> command [
	^ KMCommandModifier new
]

{ #category : #'instance creation' }
KMModifier class >> control [
	^ KMCtrlModifier new
]

{ #category : #'instance creation' }
KMModifier class >> ctrl [
	^ KMCtrlModifier new
]

{ #category : #'instance creation' }
KMModifier class >> meta [
	^ KMMetaModifier new
]

{ #category : #'instance creation' }
KMModifier class >> shift [
	^ KMShiftModifier new
]

{ #category : #combining }
KMModifier >> + modifier [
	^ modifier asKeyCombination modifiedBy: self
]

{ #category : #comparing }
KMModifier >> = aShortcut [
	super = aShortcut
		ifFalse: [ ^ false ].
	^ aShortcut eventCode = self eventCode
]

{ #category : #accessing }
KMModifier >> eventCode [
	"No modifiers"
	
	^ 0.
]

{ #category : #comparing }
KMModifier >> hash [
	^ self eventCode hash
]

{ #category : #accessing }
KMModifier >> identifier [
	^ identifier
]

{ #category : #matching }
KMModifier >> matches: aKeyboardEvent [
	self shouldNotImplement
]

{ #category : #matching }
KMModifier >> matchesEvent: aKeyboardEvent [
	self subclassResponsibility
]

{ #category : #combining }
KMModifier >> modifiedBy: modifier [
	^ KMComposedModifier new
		modifiedBy: modifier;
		modifiedBy: self;
		yourself
]

{ #category : #accessing }
KMModifier >> name [
	^ name
]

{ #category : #printing }
KMModifier >> printOn: aStream [
	aStream << name
]

{ #category : #comparing }
KMModifier >> species [

	^ KMModifier
]

{ #category : #printing }
KMModifier >> symbol [
	^ '⌘'
]
