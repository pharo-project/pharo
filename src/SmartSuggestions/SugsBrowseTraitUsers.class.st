"
I know how to browse users of a trait
"
Class {
	#name : #SugsBrowseTraitUsers,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsBrowseTraitUsers >> execute [
	SystemNavigation default browseAllUsersOfTrait: context bindedSelectedClass.

]

{ #category : #execution }
SugsBrowseTraitUsers >> isValidForContext [
	^context bindedSelectedClass isTrait
]

{ #category : #accessing }
SugsBrowseTraitUsers >> label [
	^ 'Browse Trait users' translated

]
