"
I know how to browse references from a class
"
Class {
	#name : #SugsBrowseClassReferences,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsBrowseClassReferences >> execute [
	SystemNavigation default browseAllCallsOnClass: context bindedSelectedClass.

]

{ #category : #accessing }
SugsBrowseClassReferences >> label [
	^ 'Browse references' translated

]
