"
I know how to create accessors for a class var
"
Class {
	#name : #SugsCreateClassVarAccessors,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsCreateClassVarAccessors class >> nodes [
	^{RBGlobalNode}
]

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
	^context selectedNode binding isClassVariable and: [context hasClassUnImplementedAccessors]
]

{ #category : #display }
SugsCreateClassVarAccessors >> label [
	^'Create accessors' translated
]

{ #category : #display }
SugsCreateClassVarAccessors >> position [
	^30
]
