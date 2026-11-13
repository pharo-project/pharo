Extension { #name : 'SycGenerateAllInstVarAccessorsCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycGenerateAllInstVarAccessorsCommand class >> browserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation  by: $g meta, $a meta for: ClyClass asCalypsoItemContext
]
