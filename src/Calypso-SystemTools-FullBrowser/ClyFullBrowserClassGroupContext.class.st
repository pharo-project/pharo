"
I am a context of selected class groups in full browser
"
Class {
	#name : #ClyFullBrowserClassGroupContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserClassGroupContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClassGroup
]

{ #category : #testing }
ClyFullBrowserClassGroupContext >> isClassGroupSelected [
	^self hasSelectedItems 
]

{ #category : #testing }
ClyFullBrowserClassGroupContext >> isClassTagSelected [
	| classGroupClass |
	self isClassGroupSelected ifFalse: [ ^false ].
	
	classGroupClass := self lastSelectedItem type.
	
	^classGroupClass isBasedOnClassTag
]

{ #category : #testing }
ClyFullBrowserClassGroupContext >> isPackageSelected [
	^tool packageSelection isEmpty not
]

{ #category : #'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : #'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedClassTag [
	
	^self lastSelectedObjectIn: self selectedClassTags 
]

{ #category : #'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedClassGroups [
	^self selectedObjects
]

{ #category : #'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedClassTags [
	
	^selectedItems 
		select: [ :each | each type isBasedOnClassTag ] 
		thenCollect: [ :each | each name ]
]
