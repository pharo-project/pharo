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

{ #category : 'commands' }
ClyFullBrowserProjectContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyPackageToolbarGroup)
		add: ClySwitchToPackagesCommand order: 1;
		add: ClySwitchToProjectsCommand order: 10;
		add: ClySwitchToScopedViewCommand order: 1
]

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
