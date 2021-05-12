"
I am a context of selected methods in full browser
"
Class {
	#name : #ClyFullBrowserMethodContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserMethodContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyMethod
]

{ #category : #'browser state' }
ClyFullBrowserMethodContext >> restoreBrowserState [

	self browser selectMethod: self lastSelectedMethod
]

{ #category : #'selection-methods' }
ClyFullBrowserMethodContext >> selectedMethodItems [
	^selectedItems 
]
