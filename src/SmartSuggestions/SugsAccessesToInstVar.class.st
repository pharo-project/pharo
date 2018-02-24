"
I know how to find accesses to an instance var
"
Class {
	#name : #SugsAccessesToInstVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsAccessesToInstVar class >> nodes [
	^{RBInstanceVariableNode}
]

{ #category : #execution }
SugsAccessesToInstVar >> execute [
	^SystemNavigation default browseAllAccessesTo: context selectedVariableName from: context selectedClass
]

{ #category : #display }
SugsAccessesToInstVar >> icon [
	^self iconNamed: #smallInspectItIcon
]

{ #category : #execution }
SugsAccessesToInstVar >> isValidForContext [
	^ context hasClassUnImplementedAccessors
]

{ #category : #display }
SugsAccessesToInstVar >> label [
	^'Accesses to...' translated
]

{ #category : #display }
SugsAccessesToInstVar >> position [
	^50
]
