Extension { #name : 'ToolShortcutsCategory' }

{ #category : '*Deprecated13' }
ToolShortcutsCategory >> openPlaygroundOldShortcut [

	<shortcut>
	^ KMKeymap shortcut: $o meta , $w meta action: [
		  self inform: 'CMD/CTRL + O + W shortcut to open the playground has been deprecated in favor of CMD/CTRL + O + P'.
		  self tools workspace open ]
]
