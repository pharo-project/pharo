"
I am a context of selected projects in full browser
"
Class {
	#name : #ClyFullBrowserProjectContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserProjectContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyProjectChildItem
]

{ #category : #testing }
ClyFullBrowserProjectContext >> isClassGroupSelected [
	^false
]

{ #category : #testing }
ClyFullBrowserProjectContext >> isClassTagSelected [
	^false
]

{ #category : #'selection-projects' }
ClyFullBrowserProjectContext >> selectedProjectItems [
	
	^selectedItems collect: [ :each | each rootParentItem ]
]
