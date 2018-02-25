"
I know how to replace the call of a class variable for the use of an accessor and create accessors if it's necessary.
"
Class {
	#name : #SugsUseAccessorsForClassVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsUseAccessorsForClassVar class >> nodes [
	^{RBGlobalNode}
]

{ #category : #execution }
SugsUseAccessorsForClassVar >> execute [
	(NautilusRefactoring model: context) abstractClassVarFrom: context selectedClass
]

{ #category : #display }
SugsUseAccessorsForClassVar >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #execution }
SugsUseAccessorsForClassVar >> isValidForContext [
	^context selectedNode binding isClassVariable
]

{ #category : #display }
SugsUseAccessorsForClassVar >> label [
	^'Use accessor' translated
]

{ #category : #display }
SugsUseAccessorsForClassVar >> position [
	^10
]
