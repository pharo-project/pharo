"
I am a context of selected projects in full browser
"
Class {
	#name : 'ClyFullBrowserProjectContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'selection strategy' }
ClyFullBrowserProjectContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: Project
]

{ #category : 'testing' }
ClyFullBrowserProjectContext >> isClassGroupSelected [
	^false
]

{ #category : 'testing' }
ClyFullBrowserProjectContext >> isPackageTagSelected [
	^false
]
