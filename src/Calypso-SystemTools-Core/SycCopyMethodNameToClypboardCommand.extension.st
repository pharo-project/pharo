Extension { #name : #SycCopyMethodNameToClypboardCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyMethodNameToClypboardCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^SycMethodMenuActivation byItemOf: CmdExtraMenuGroup for: ClyMethod asCalypsoItemContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycCopyMethodNameToClypboardCommand class >> methodTabIconActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext
]
