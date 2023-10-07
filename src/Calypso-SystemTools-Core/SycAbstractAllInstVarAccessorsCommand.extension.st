Extension { #name : 'SycAbstractAllInstVarAccessorsCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycAbstractAllInstVarAccessorsCommand class >> browserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation  by: $a meta, $a meta for: ClyClass asCalypsoItemContext
]
