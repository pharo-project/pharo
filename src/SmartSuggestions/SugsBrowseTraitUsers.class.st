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

{ #category : #position }
SugsBrowseTraitUsers >> icon [
	^self iconNamed: #smallInspectItIcon
]

{ #category : #execution }
SugsBrowseTraitUsers >> isValidForContext [
	^context bindedSelectedClass isTrait
]

{ #category : #position }
SugsBrowseTraitUsers >> label [
	^ 'Browse Trait users' translated

]

{ #category : #position }
SugsBrowseTraitUsers >> position [
	^10
]
