"
I am a context of selected method groups in full browser
"
Class {
	#name : 'ClyFullBrowserMethodGroupContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'selection strategy' }
ClyFullBrowserMethodGroupContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyMethodGroup
]

{ #category : 'accessing' }
ClyFullBrowserMethodGroupContext >> commanderCommandClasses [

	^ super commanderCommandClasses , {
		  SycClassCommand.
		  ClyMethodGroupCommand }
]

{ #category : 'testing' }
ClyFullBrowserMethodGroupContext >> isFullBrowserMethodGroupContext [

	^ true
]

{ #category : 'selection-method groups' }
ClyFullBrowserMethodGroupContext >> selectedMethodGroupItems [
	^selectedItems
]
