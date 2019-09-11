Extension { #name : #SycCopyMethodNameToClypboardCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyMethodNameToClypboardCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyMethod asCalypsoItemContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyMethodNameToClypboardCommand class >> methodTabIconActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext
]
