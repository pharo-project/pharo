"
I am a context of selected packages in full browser
"
Class {
	#name : #ClyPackageContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyPackageContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: RPackage
]

{ #category : #'selection-class groups' }
ClyPackageContextOfFullBrowser >> isClassGroupSelected [
	^false
]

{ #category : #'selection-class groups' }
ClyPackageContextOfFullBrowser >> isClassTagSelected [
	^false
]

{ #category : #public }
ClyPackageContextOfFullBrowser >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : #'selection-packages' }
ClyPackageContextOfFullBrowser >> selectedPackageItems [
	^selectedItems
]
