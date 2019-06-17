Extension { #name : #CmdCommandMenuItem }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandMenuItem >> buildContextMenu: aMenuView [
	
	activator buildContextMenu: aMenuView
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandMenuItem >> registerContextMenuItemsWithBuilder: aBuilder [

	activator registerContextMenuItemsFor: self withBuilder: aBuilder
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommandMenuItem >> setUpShortcutTipForMenuItem: aMenuItemMorph [

	activator setUpShortcutTipForMenuItem: aMenuItemMorph
]
