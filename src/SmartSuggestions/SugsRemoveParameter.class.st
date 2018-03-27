"
I know how to remove a parameter from a method
"
Class {
	#name : #SugsRemoveParameter,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsRemoveParameter class >> nodes [
	^{RBMethodNode}
]

{ #category : #execution }
SugsRemoveParameter >> execute [
	(NautilusRefactoring model: context) removeParameterFor: context selectedMethod 
]

{ #category : #display }
SugsRemoveParameter >> icon [
	^self iconNamed: #smallCancelIcon
]

{ #category : #execution }
SugsRemoveParameter >> isValidForContext [
	^ context hasParameters
]

{ #category : #display }
SugsRemoveParameter >> label [
	^'Remove parameter' translated
]

{ #category : #display }
SugsRemoveParameter >> position [
	^210
]
