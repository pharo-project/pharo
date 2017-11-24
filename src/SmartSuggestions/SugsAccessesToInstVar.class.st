"
I know how to find accesses to an instance var
"
Class {
	#name : #SugsAccessesToInstVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsAccessesToInstVar >> execute [
	^SystemNavigation default browseAllAccessesTo: context selectedVariableName from: context selectedClass
]

{ #category : #execution }
SugsAccessesToInstVar >> isValidForContext [
	^ context hasClassUnImplementedAccessors
]

{ #category : #accessing }
SugsAccessesToInstVar >> label [
	^'Accesses to...' translated
]
