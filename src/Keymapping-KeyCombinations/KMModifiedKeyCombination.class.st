"
I represent key combinations combining a single key + a modifier.

The modifier could be a single modifier key (look at my subclasses) or a combination of them. For example, valid modified key combinations could be:

- shift + a
- ctrl + shift + c

To create a modified key combinations use the #command, #alt, #control or #shift messages. Like for example:

$a command
$b shift
$1 control
$z alt
"
Class {
	#name : #KMModifiedKeyCombination,
	#superclass : #KMKeyCombination,
	#instVars : [
		'modifier',
		'character'
	],
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #'instance creation' }
KMModifiedKeyCombination class >> modifier: aModifier character: aCharacter [
	^ (self new)
		modifier: aModifier;
		character: aCharacter;
		yourself
]

{ #category : #comparing }
KMModifiedKeyCombination >> = aShortcut [
	super = aShortcut
		ifFalse: [ ^ false ]. 
	 modifier = aShortcut modifier
		ifFalse: [ ^ false ].
	aShortcut character = character
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #combining }
KMModifiedKeyCombination >> alt [
	^ self modifiedBy: KMModifier alt
]

{ #category : #matching }
KMModifiedKeyCombination >> beginsWith: aKMShortcut [ 

	^ self = aKMShortcut 
]

{ #category : #accessing }
KMModifiedKeyCombination >> character [
	^ character
]

{ #category : #accessing }
KMModifiedKeyCombination >> character: aCharacter [ 
	character := aCharacter.
]

{ #category : #combining }
KMModifiedKeyCombination >> command [
	^ self modifiedBy: KMModifier command
]

{ #category : #combining }
KMModifiedKeyCombination >> control [
	^ self modifiedBy: KMModifier ctrl
]

{ #category : #combining }
KMModifiedKeyCombination >> ctrl [
	^ self control
]

{ #category : #comparing }
KMModifiedKeyCombination >> hash [
	^ character hash + modifier hash
]

{ #category : #accessing }
KMModifiedKeyCombination >> key [

	^ self character key.
]

{ #category : #matching }
KMModifiedKeyCombination >> matchesEvent: aKeyboardEvent [

	^ (self modifier matchesEvent: aKeyboardEvent) 
		and: [self character matchesEvent: aKeyboardEvent]
]

{ #category : #combining }
KMModifiedKeyCombination >> meta [
	^ self modifiedBy: KMModifier meta
]

{ #category : #combining }
KMModifiedKeyCombination >> modifiedBy: aModifier [
	^self character asKeyCombination modifiedBy: modifier + aModifier.
]

{ #category : #accessing }
KMModifiedKeyCombination >> modifier [
	^ modifier
]

{ #category : #accessing }
KMModifiedKeyCombination >> modifier: aModifier [
	modifier := aModifier
]

{ #category : #'accessing platform' }
KMModifiedKeyCombination >> platformCharacter [
	
	^ self character key
]

{ #category : #'accessing platform' }
KMModifiedKeyCombination >> platformModifier [
	
	^ self modifier
]

{ #category : #printing }
KMModifiedKeyCombination >> printOn: aStream [
	modifier printOn: aStream.
	aStream << ' + '.
	character printOn: aStream.
]

{ #category : #combining }
KMModifiedKeyCombination >> shift [
	^ self modifiedBy: KMModifier shift
]
