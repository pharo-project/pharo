Extension { #name : #SycGeneratePrintStringCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycGeneratePrintStringCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyClassContextOfFullBrowser
]
