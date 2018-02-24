"
I know how to create accessors for a class var
"
Class {
	#name : #SugsCreateClassVarAccessors,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsCreateClassVarAccessors >> execute [
	(NautilusRefactoring model: context) accessorsClassVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #display }
SugsCreateClassVarAccessors >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #execution }
SugsCreateClassVarAccessors >> isValidForContext [
	^context hasClassUnImplementedAccessors 
]

{ #category : #display }
SugsCreateClassVarAccessors >> label [
	^'Create accessors' translated
]

{ #category : #display }
SugsCreateClassVarAccessors >> position [
	^30
]
