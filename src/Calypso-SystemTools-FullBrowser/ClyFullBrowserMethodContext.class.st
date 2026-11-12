"
I am a context of selected methods in full browser
"
Class {
	#name : 'ClyFullBrowserMethodContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserMethodContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyClassSideLinkTableDecorator.
	aCommandMap shortcuts add: ClyFindMethodInBrowserCommand key: $f meta.
	aCommandMap tableDecorators add: ClyClassSideBoldTableDecorator
]

{ #category : 'selection strategy' }
ClyFullBrowserMethodContext class >> selectionContextDeclaration [

	<calypsoSelectionContext>
	^ ClySelectionContextDeclaration browser: ClyFullBrowserMorph itemType: ClyMethod
]

{ #category : 'testing' }
ClyFullBrowserMethodContext >> isFullBrowserMethodContext [

	^ true
]

{ #category : 'browser state' }
ClyFullBrowserMethodContext >> restoreBrowserState [

	self browser selectMethod: self lastSelectedMethod
]

{ #category : 'selection-methods' }
ClyFullBrowserMethodContext >> selectedMethodItems [
	^selectedItems
]
