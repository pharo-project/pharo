"
I am a context of selected projects in full browser
"
Class {
	#name : #ClyProjectContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyProjectContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyProjectChildItem
]

{ #category : #testing }
ClyProjectContextOfFullBrowser >> isClassGroupSelected [
	^false
]

{ #category : #testing }
ClyProjectContextOfFullBrowser >> isClassTagSelected [
	^false
]

{ #category : #'selection-projects' }
ClyProjectContextOfFullBrowser >> selectedProjectItems [
	
	^selectedItems collect: [ :each | each rootParentItem ]
]
