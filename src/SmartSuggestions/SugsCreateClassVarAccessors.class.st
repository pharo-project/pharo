"
I know how to create accessors for a class var
"
Class {
	#name : #SugsCreateClassVarAccessors,
	#superclass : #SugsSuggestion,
	#category : #SmartSuggestions-Suggestion
}

{ #category : #execution }
SugsCreateClassVarAccessors >> execute [
	(NautilusRefactoring model: context) accessorsClassVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #execution }
SugsCreateClassVarAccessors >> isValidForContext [
	^context hasClassUnImplementedAccessors 
]

{ #category : #accessing }
SugsCreateClassVarAccessors >> label [
	^'Create accessors' translated
]
