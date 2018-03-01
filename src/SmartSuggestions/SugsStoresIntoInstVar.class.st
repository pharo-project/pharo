"
I know how to find stores into for an instance var
"
Class {
	#name : #SugsStoresIntoInstVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsStoresIntoInstVar >> execute [
	 SystemNavigation default browseAllStoresInto: context selectedVariableName from: context selectedClass
]

{ #category : #accessing }
SugsStoresIntoInstVar >> label [
	^'Stores Into...' translated
]
