Extension { #name : 'SycAddPackagesScopeCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddPackagesScopeCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserPackageContext
]
