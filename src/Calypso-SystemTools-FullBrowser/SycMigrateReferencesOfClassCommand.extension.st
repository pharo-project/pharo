Extension { #name : 'SycMigrateReferencesOfClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycMigrateReferencesOfClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1.4 for: ClyFullBrowserClassContext
]
