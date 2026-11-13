Extension { #name : 'ClySpawnFullBrowserCommand' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClySpawnFullBrowserCommand class >> queryBrowserDoubleClickMenuActivation [
	<classAnnotation>

	^ClyDoubleClickActivation for: ClyQueryBrowserContext
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClySpawnFullBrowserCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>

	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyQueryBrowserContext
]
