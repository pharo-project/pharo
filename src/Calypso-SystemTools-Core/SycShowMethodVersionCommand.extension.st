Extension { #name : 'SycShowMethodVersionCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycShowMethodVersionCommand class >> methodBrowserTabActivation [
	<classAnnotation>

	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycShowMethodVersionCommand class >> methodShortcutActivation [
    <classAnnotation>
    ^ ClyShortcutActivation by: $v meta for: ClyMethod asCalypsoItemContext

]
