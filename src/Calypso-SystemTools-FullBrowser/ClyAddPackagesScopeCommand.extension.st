Extension { #name : #ClyAddPackagesScopeCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyAddPackagesScopeCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserPackageContext 
]
