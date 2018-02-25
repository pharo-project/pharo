"
I'm the representation from a suggestion that can be executed.

How to add a AST Menu:

1) make a subclass of SugsSuggestion
2) on the class side, #node returns the AST nodes you are interested in
3) instance side:

execute
	the action to be performed
label
	the string used in the menu

optional
	icon		
	position
	
"
Class {
	#name : #SugsSuggestion,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #'instance creation' }
SugsSuggestion class >> for: aClass [
	^(self subclassesForNode: aClass) collect: #new
]

{ #category : #accessing }
SugsSuggestion class >> nodes [
	^{RBProgramNode}
]

{ #category : #private }
SugsSuggestion class >> subclassesForNode: aClass [
	"all menu defitions define in which ast nodes they are interested in #nodes"
	^self allSubclasses select: [:suggestionClass |  
		suggestionClass nodes anySatisfy: [:class | aClass == class or: [aClass inheritsFrom: class]]]
]

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
