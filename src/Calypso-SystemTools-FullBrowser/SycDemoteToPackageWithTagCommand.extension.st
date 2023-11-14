Extension { #name : 'SycDemoteToPackageWithTagCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDemoteToPackageWithTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: Package asCalypsoItemContext
]
