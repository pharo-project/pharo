Extension { #name : 'SycRenamePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenamePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation renamingFor: ClyFullBrowserPackageContext
]
