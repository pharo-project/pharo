Extension { #name : #SycRecompilePackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRecompilePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: RPackage asCalypsoItemContext
]
