"
I know how to replace the call of a class variable for the use of an accessor and create accessors if it's necessary.
"
Class {
	#name : #SugsUseAccessorsForClassVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsUseAccessorsForClassVar >> execute [
	(NautilusRefactoring model: context) abstractClassVarFrom: context selectedClass
]

{ #category : #accessing }
SugsUseAccessorsForClassVar >> label [
	^'Use accessor' translated
]
