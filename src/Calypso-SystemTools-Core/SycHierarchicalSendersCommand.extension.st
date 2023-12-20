Extension { #name : 'SycHierarchicalSendersCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycHierarchicalSendersCommand class >> browserMenuOrder [
	^ 2
]

{ #category : '*Calypso-SystemTools-Core' }
SycHierarchicalSendersCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycHierarchicalSendersCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycHierarchicalSendersCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
