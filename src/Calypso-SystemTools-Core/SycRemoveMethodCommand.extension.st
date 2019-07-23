Extension { #name : #SycRemoveMethodCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRemoveMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyMethod asCalypsoItemContext
]
