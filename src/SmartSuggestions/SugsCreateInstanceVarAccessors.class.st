"
I know how to create accessors for an instance var
"
Class {
	#name : #SugsCreateInstanceVarAccessors,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsCreateInstanceVarAccessors class >> nodes [
	^{RBInstanceVariableNode}
]

{ #category : #execution }
SugsCreateInstanceVarAccessors >> execute [
	(NautilusRefactoring model: context) accessorsInstVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #display }
SugsCreateInstanceVarAccessors >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #execution }
SugsCreateInstanceVarAccessors >> isValidForContext [
	^ context hasUnImplementedAccessors
]

{ #category : #display }
SugsCreateInstanceVarAccessors >> label [
	^ 'Create Accesors' translated
]

{ #category : #display }
SugsCreateInstanceVarAccessors >> position [
	^100
]
