"
I know how to browse references from a class
"
Class {
	#name : #SugsBrowseClassReferences,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsBrowseClassReferences class >> nodes [
	^{RBGlobalNode}
]

{ #category : #execution }
SugsBrowseClassReferences >> execute [
	SystemNavigation default browseAllCallsOnClass: context bindedSelectedClass.

]

{ #category : #accessing }
SugsBrowseClassReferences >> icon [
	^self iconNamed: #smallInspectItIcon
]

{ #category : #execution }
SugsBrowseClassReferences >> isValidForContext [
	^context selectedNode binding value isClass
]

{ #category : #accessing }
SugsBrowseClassReferences >> label [
	^ 'Browse references' translated

]

{ #category : #accessing }
SugsBrowseClassReferences >> position [
	^10
]
