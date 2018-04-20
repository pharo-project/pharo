Extension { #name : #ToolShortcutsCategory }

{ #category : #'*Komitter' }
ToolShortcutsCategory >> openKomitter [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openKomitterShortcut action: [ Komitter openAndCommitToMonticello ]
]
