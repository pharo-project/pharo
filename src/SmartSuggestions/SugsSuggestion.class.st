"
I'm the representation from a suggestion that can be executed.
I have some information related to the suggestion.
"
Class {
	#name : #SugsSuggestion,
	#superclass : #Object,
	#instVars : [
		'blockCommand',
		'context',
		'label',
		'icon',
		'position',
		'keyText',
		'isValidBlock'
	],
	#category : #SmartSuggestions-Suggestion
}

{ #category : #'instance creation' }
SugsSuggestion class >> for: blockCommand named: label icon: icon [ 
	^ self new 
		blockCommand: blockCommand;
		label: label;
		icon: icon;
		yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> for: blockCommand named: label icon: icon position: position [
	^ (self for: blockCommand named: label icon: icon)
	 	  position: position;
		  yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> for: blockCommand named: label icon: icon position: position isValidBlock: validBlock [
	^ (self for: blockCommand named: label icon: icon position: position)
   		  isValidBlock: validBlock;
		  yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> for: blockCommand named: label icon: icon position: position keyText: key [
	^ (self for: blockCommand named: label icon: icon position: position)
		  keyText: key;
		  yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> for: blockCommand named: label icon: icon position: position keyText: key  isValidBlock: validBlock [
	^ (self for: blockCommand named: label icon: icon position: position)
		  keyText: key;
		 isValidBlock: validBlock;
		  yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> icon: icon position: position [
	^ self new
	 	icon: icon;	 
		position: position;
		yourself.
]

{ #category : #'instance creation' }
SugsSuggestion class >> icon: icon position: position keyText: key [
	^ self new
	 	icon: icon;	 
		position: position;
		keyText: key;
		yourself.
]

{ #category : #comparing }
SugsSuggestion >> = otherSuggestion [
	otherSuggestion class = self class ifFalse: [  ^false ].
	^  self label = otherSuggestion label.
]

{ #category : #private }
SugsSuggestion >> blockCommand: aBlock [ 
	blockCommand := aBlock.
]

{ #category : #private }
SugsSuggestion >> context: theContext [ 
	context := theContext .
	
]

{ #category : #execution }
SugsSuggestion >> execute [
	blockCommand value: context.
]

{ #category : #display }
SugsSuggestion >> icon [
	^icon.
]

{ #category : #private }
SugsSuggestion >> icon: anIcon [
	icon := anIcon 
]

{ #category : #private }
SugsSuggestion >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #private }
SugsSuggestion >> isValidBlock: aBlock [ 
	isValidBlock := aBlock.
]

{ #category : #execution }
SugsSuggestion >> isValidForContext [
	^ isValidBlock ifNil:[true] ifNotNil:[isValidBlock value: context].
]

{ #category : #display }
SugsSuggestion >> keyText [
	^ keyText
]

{ #category : #display }
SugsSuggestion >> keyText: key [
	keyText := key.
]

{ #category : #display }
SugsSuggestion >> label [
	^label
]

{ #category : #private }
SugsSuggestion >> label: aLabel [
	label := aLabel.
]

{ #category : #display }
SugsSuggestion >> position [
	^ position ifNil:[10000] ifNotNil: [ position ].
]

{ #category : #private }
SugsSuggestion >> position: aPosition [
	position := aPosition .
]

{ #category : #printing }
SugsSuggestion >> printOn: aStream [
	aStream
		nextPutAll: 'aSugsSuggestion (';
		nextPutAll: self label;
		nextPutAll: ')'.
]
