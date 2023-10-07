Extension { #name : 'SycSuperclassImplementorsCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycSuperclassImplementorsCommand class >> browserMenuOrder [
	^2
]

{ #category : '*Calypso-SystemTools-Core' }
SycSuperclassImplementorsCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycSuperclassImplementorsCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycSuperclassImplementorsCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
