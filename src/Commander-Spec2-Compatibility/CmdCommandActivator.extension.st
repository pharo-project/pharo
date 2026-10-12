Extension { #name : 'CmdCommandActivator' }

{ #category : '*Commander-Spec2-Compatibility' }
CmdCommandActivator >> getShortcutTipForMenuItem [

	CmdShortcutActivation
		activeInstancesFor: self command class
		inContext: self context
		do: [ :shortcut | "this is trick to show shortcut on menu with existing menu support" ^ shortcut keyCombination ].
	^ nil
]

{ #category : '*Commander-Spec2-Compatibility' }
CmdCommandActivator >> help [
	^ self command class defaultHelp
]
