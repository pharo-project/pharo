Extension { #name : #CmdCommandMenuItem }

{ #category : #'*Commander-Spec2-Compatibility' }
CmdCommandMenuItem >> addToMenuPresenter: aSpMenuPresenter [
	aSpMenuPresenter
		addItem: [ :anItem | 
			anItem
				name: self name;
				icon: self icon;
				action: [ self executeCommand ];
				enabled: self isActive;
				description: self command class defaultHelp.
				self getShortcutTipForMenuItem ifNotNil: [ :shortcut | anItem shortcut: shortcut ] ]
]

{ #category : #'*Commander-Spec2-Compatibility' }
CmdCommandMenuItem >> getShortcutTipForMenuItem [
 	^activator getShortcutTipForMenuItem
]
