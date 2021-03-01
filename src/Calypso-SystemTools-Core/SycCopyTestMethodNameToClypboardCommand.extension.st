Extension { #name : #SycCopyTestMethodNameToClypboardCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyTestMethodNameToClypboardCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^SycMethodMenuActivation byItemOf: CmdExtraMenuGroup for: ClyMethod asCalypsoItemContext
]
