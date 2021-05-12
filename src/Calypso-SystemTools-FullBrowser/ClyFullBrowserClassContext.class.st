"
I am a context of selected classes in full browser
"
Class {
	#name : #ClyFullBrowserClassContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserClassContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClass
]

{ #category : #'browser state' }
ClyFullBrowserClassContext >> restoreBrowserState [

	self browser selectClass: self lastSelectedClass
]

{ #category : #'selection-classes' }
ClyFullBrowserClassContext >> selectedClassItems [
	^selectedItems
]
