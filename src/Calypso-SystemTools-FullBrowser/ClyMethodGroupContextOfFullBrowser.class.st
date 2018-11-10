"
I am a context of selected method groups in full browser
"
Class {
	#name : #ClyMethodGroupContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyMethodGroupContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyMethodGroup
]

{ #category : #'selection-method groups' }
ClyMethodGroupContextOfFullBrowser >> selectedMethodGroupItems [
	^selectedItems
]
