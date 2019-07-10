"
I am a context of selected methods in full browser
"
Class {
	#name : #ClyMethodContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyMethodContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyMethod
]

{ #category : #'browser state' }
ClyMethodContextOfFullBrowser >> restoreBrowserState [

	self browser selectMethod: self lastSelectedMethod
]

{ #category : #'selection-methods' }
ClyMethodContextOfFullBrowser >> selectedMethodItems [
	^selectedItems 
]
