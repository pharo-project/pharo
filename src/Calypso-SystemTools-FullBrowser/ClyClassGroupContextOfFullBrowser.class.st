"
I am a context of selected class groups in full browser
"
Class {
	#name : #ClyClassGroupContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyClassGroupContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyClassGroup
]

{ #category : #testing }
ClyClassGroupContextOfFullBrowser >> isClassGroupSelected [
	^self hasSelectedItems 
]

{ #category : #testing }
ClyClassGroupContextOfFullBrowser >> isClassTagSelected [
	| classGroupClass |
	self isClassGroupSelected ifFalse: [ ^false ].
	
	classGroupClass := self lastSelectedItem type.
	
	^classGroupClass isBasedOnClassTag
]

{ #category : #testing }
ClyClassGroupContextOfFullBrowser >> isPackageSelected [
	^tool packageSelection isEmpty not
]

{ #category : #'selection-class groups' }
ClyClassGroupContextOfFullBrowser >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : #'selection-class groups' }
ClyClassGroupContextOfFullBrowser >> lastSelectedClassTag [
	
	^self lastSelectedObjectIn: self selectedClassTags 
]

{ #category : #'selection-class groups' }
ClyClassGroupContextOfFullBrowser >> selectedClassGroups [
	^self selectedObjects
]

{ #category : #'selection-class groups' }
ClyClassGroupContextOfFullBrowser >> selectedClassTags [
	
	^selectedItems 
		select: [ :each | each type isBasedOnClassTag ] 
		thenCollect: [ :each | each name ]
]
