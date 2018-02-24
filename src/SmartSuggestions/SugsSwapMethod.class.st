"
I know how to move a method from instance side to class side and the oposit, depending the method.
"
Class {
	#name : #SugsSwapMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsSwapMethod class >> node [
	^{RBMethodNode}
]

{ #category : #execution }
SugsSwapMethod >> execute [
	^(NautilusRefactoring model: context) 
		swapMethodFor: (OrderedCollection with: context selectedMethod).
]

{ #category : #display }
SugsSwapMethod >> icon [
	^self iconNamed: #smallRedoIcon
]

{ #category : #display }
SugsSwapMethod >> label [
	
	^ context selectedMethod 
		ifNil: ['Swap method' translated] 
		ifNotNil: [context selectedMethod methodClass isMeta ifTrue: ['Move to instance side' translated] ifFalse:['Move to class side' translated]]

]

{ #category : #display }
SugsSwapMethod >> position [
	^50
]
