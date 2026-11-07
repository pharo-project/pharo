"
I am a context of selected classes in full browser
"
Class {
	#name : 'ClyFullBrowserClassContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'selection strategy' }
ClyFullBrowserClassContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClass
]

{ #category : 'testing' }
ClyFullBrowserClassContext >> commanderCommandClasses [

	^ super commanderCommandClasses , { SycClassCommand }
]

{ #category : 'testing' }
ClyFullBrowserClassContext >> isFullBrowserClassContext [

	^ true
]

{ #category : 'browser state' }
ClyFullBrowserClassContext >> restoreBrowserState [

	self browser selectClass: self lastSelectedClass
]

{ #category : 'selection-classes' }
ClyFullBrowserClassContext >> selectedClassItems [
	^selectedItems
]
