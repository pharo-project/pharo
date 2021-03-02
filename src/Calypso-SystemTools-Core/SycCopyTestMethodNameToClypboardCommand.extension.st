Extension { #name : #SycCopyTestMethodNameToClypboardCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyTestMethodNameToClypboardCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyMethod asCalypsoItemContext
]
