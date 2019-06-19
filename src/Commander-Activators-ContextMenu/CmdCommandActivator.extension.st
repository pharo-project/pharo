Extension { #name : #CmdCommandActivator }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandActivator >> buildContextMenu: aMenu [
	
	self canExecuteCommand ifFalse: [ ^self ].
	
	command fillContextMenu: aMenu using: self
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandActivator >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [
	
	self canExecuteCommand ifFalse: [ ^self ].
	
	command registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandActivator >> setUpShortcutTipForMenuItem: aMenuItemMorph [
	| keyText |
	
	CmdShortcutActivation 
		activeInstancesFor: command class inContext: context
		do: [ :shortcut |  	
			"this is trick to show shortcut on menu with existing menu support"
			keyText := String streamContents: [:s | 
				shortcut keyCombination prettyPrintOn: s].
			keyText := keyText copyWithout: keyText first.
			aMenuItemMorph keyText: keyText]
]
