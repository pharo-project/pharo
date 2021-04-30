"
I am a context of selected method groups in full browser
"
Class {
	#name : #ClyFullBrowserMethodGroupContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserMethodGroupContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyMethodGroup
]

{ #category : #'selection-method groups' }
ClyFullBrowserMethodGroupContext >> selectedMethodGroupItems [
	^selectedItems
]
