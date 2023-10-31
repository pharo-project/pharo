"
I am a context of selected class groups in full browser
"
Class {
	#name : 'ClyFullBrowserClassGroupContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'selection strategy' }
ClyFullBrowserClassGroupContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClassGroup
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isClassGroupSelected [
	^self hasSelectedItems
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isPackageSelected [
	^tool packageSelection isEmpty not
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isPackageTagSelected [
	| classGroupClass |
	self isClassGroupSelected ifFalse: [ ^false ].

	classGroupClass := self lastSelectedItem type.

	^classGroupClass isBasedOnPackageTag
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedPackageTag [

	^self lastSelectedObjectIn: self selectedPackageTags
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedClassGroups [
	^self selectedObjects
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedPackageTags [

	^selectedItems
		select: [ :each | each type isBasedOnPackageTag ]
		thenCollect: [ :each | each name ]
]
