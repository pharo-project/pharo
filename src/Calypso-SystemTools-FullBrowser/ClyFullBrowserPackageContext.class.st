"
I am a context of selected packages in full browser
"
Class {
	#name : 'ClyFullBrowserPackageContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'selection strategy' }
ClyFullBrowserPackageContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: Package
]

{ #category : 'selection-class groups' }
ClyFullBrowserPackageContext >> isClassGroupSelected [
	^false
]

{ #category : 'selection-class groups' }
ClyFullBrowserPackageContext >> isPackageTagSelected [
	^false
]

{ #category : 'public' }
ClyFullBrowserPackageContext >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : 'selection-packages' }
ClyFullBrowserPackageContext >> selectedPackageItems [
	^selectedItems
]
