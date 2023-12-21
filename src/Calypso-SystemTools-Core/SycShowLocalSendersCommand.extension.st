Extension { #name : 'SycShowLocalSendersCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> browserMenuOrder [
	^ 2
]

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder 
		for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation 
		by: $v meta
		for: ClyMethodSourceCodeContext
]
