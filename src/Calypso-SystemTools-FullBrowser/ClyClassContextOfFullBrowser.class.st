"
I am a context of selected classes in full browser
"
Class {
	#name : #ClyClassContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyClassContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyClass
]

{ #category : #'browser state' }
ClyClassContextOfFullBrowser >> restoreBrowserState [

	self browser selectClass: self lastSelectedClass
]

{ #category : #'selection-classes' }
ClyClassContextOfFullBrowser >> selectedClassItems [
	^selectedItems
]
