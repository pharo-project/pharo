Extension { #name : #SycGenerateEqualAndHashCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycGenerateEqualAndHashCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyClassContextOfFullBrowser
]
