"
I know how to find stores into for an instance var
"
Class {
	#name : #SugsStoresIntoInstVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsStoresIntoInstVar class >> nodes [
	^{RBInstanceVariableNode}
]

{ #category : #execution }
SugsStoresIntoInstVar >> execute [
	 SystemNavigation default browseAllStoresInto: context selectedVariableName from: context selectedClass
]

{ #category : #display }
SugsStoresIntoInstVar >> icon [
	^self iconNamed: #smallInspectItIcon
]

{ #category : #display }
SugsStoresIntoInstVar >> label [
	^'Stores Into...' translated
]

{ #category : #display }
SugsStoresIntoInstVar >> position [
	^60
]
