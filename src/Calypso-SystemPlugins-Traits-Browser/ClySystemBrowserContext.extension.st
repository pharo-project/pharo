Extension { #name : #ClySystemBrowserContext }

{ #category : #'*Calypso-SystemPlugins-Traits-Browser' }
ClySystemBrowserContext >> isSelectedClassUsesTraits [
	| classItem |
	classItem := self lastSelectedObjectIn: self selectedClassItems.
	
	^classItem isMarkedWith: ClyTraitUserTag
]

{ #category : #'*Calypso-SystemPlugins-Traits-Browser' }
ClySystemBrowserContext >> isTraitSelected [
	| classItem |
	classItem := self lastSelectedObjectIn: self selectedClassItems.
	^classItem isMarkedWith: ClyTraitTag
]
