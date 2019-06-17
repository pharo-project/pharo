Extension { #name : #ClySystemBrowserContext }

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClySystemBrowserContext >> browseVariableReferencesOf: variables [

	tool spawnQueryBrowserOn: (ClyVariableReferencesQuery ofAny: variables )
]
