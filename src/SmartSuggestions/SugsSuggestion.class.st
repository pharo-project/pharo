"
I'm the representation from a suggestion that can be executed.
I have some information related to the suggestion.
"
Class {
	#name : #SugsSuggestion,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #comparing }
SugsSuggestion >> = otherSuggestion [
	otherSuggestion class = self class ifFalse: [  ^false ].
	^  self label = otherSuggestion label.
]

{ #category : #private }
SugsSuggestion >> context: theContext [ 
	context := theContext .
	
]

{ #category : #execution }
SugsSuggestion >> execute [
	self subclassResponsibility 
]

{ #category : #display }
SugsSuggestion >> icon [
	^nil
]

{ #category : #private }
SugsSuggestion >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #execution }
SugsSuggestion >> isValidForContext [
	^ true
]

{ #category : #display }
SugsSuggestion >> keyText [
	^nil
]

{ #category : #display }
SugsSuggestion >> label [
	^nil
]

{ #category : #display }
SugsSuggestion >> position [
	^1000
]

{ #category : #printing }
SugsSuggestion >> printOn: aStream [
	aStream
		nextPutAll: 'aSugsSuggestion (';
		nextPutAll: self label;
		nextPutAll: ')'.
]
