Extension { #name : 'SycRemoveClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemoveClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation removalFor: ClyFullBrowserClassContext
]
