Extension { #name : #SycRenameMessageInSomePackagesCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameMessageInSomePackagesCommand class >> browserMenuOrder [
	^ 3
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameMessageInSomePackagesCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $k meta for: ClyMethod asCalypsoItemContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameMessageInSomePackagesCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation  by: $k meta for: ClyMethodSourceCodeContext
]
