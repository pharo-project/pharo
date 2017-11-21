"
I know how to create accessors for an instance var
"
Class {
	#name : #SugsCreateInstanceVarAccessors,
	#superclass : #SugsSuggestion,
	#category : #SmartSuggestions-Suggestion
}

{ #category : #execution }
SugsCreateInstanceVarAccessors >> execute [
	(NautilusRefactoring model: context) accessorsInstVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #execution }
SugsCreateInstanceVarAccessors >> isValidForContext [
	^ context hasUnImplementedAccessors
]

{ #category : #accessing }
SugsCreateInstanceVarAccessors >> label [
	^ 'Create Accesors' translated
]
