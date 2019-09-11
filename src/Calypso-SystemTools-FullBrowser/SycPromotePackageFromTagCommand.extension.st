Extension { #name : #SycPromotePackageFromTagCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPromotePackageFromTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyTaggedClassGroup asCalypsoItemContext
]
