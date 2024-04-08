Extension { #name : 'ToolShortcutsCategory' }

{ #category : '*Deprecated12' }
ToolShortcutsCategory >> openWorkspace [

	<shortcut>
	^ KMKeymap shortcut: $o meta, $w meta action: [ self tools workspace open ]
]
