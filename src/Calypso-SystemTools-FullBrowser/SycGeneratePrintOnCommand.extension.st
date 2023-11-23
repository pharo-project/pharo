Extension { #name : 'SycGeneratePrintOnCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycGeneratePrintOnCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyFullBrowserClassContext
]
