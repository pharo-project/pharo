"
I know how to move a method from instance side to class side and the oposit, depending the method.
"
Class {
	#name : #SugsSwapMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsSwapMethod >> execute [
	^(NautilusRefactoring model: context) 
		swapMethodFor: (OrderedCollection with: context selectedMethod).
]

{ #category : #accessing }
SugsSwapMethod >> icon [
	^self iconNamed: #smallRedoIcon
]

{ #category : #accessing }
SugsSwapMethod >> label [
	
	^ context selectedMethod 
		ifNil: ['Swap method' translated] 
		ifNotNil: [context selectedMethod methodClass isMeta ifTrue: ['Move to instance side' translated] ifFalse:['Move to class side' translated]]

]

{ #category : #accessing }
SugsSwapMethod >> position [
	^50
]
