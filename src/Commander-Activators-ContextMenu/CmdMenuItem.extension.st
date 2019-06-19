Extension { #name : #CmdMenuItem }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuItem >> buildContextMenu: aMenu [

	self subclassResponsibility 
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuItem >> registerContextMenuItemsWithBuilder: aBuilder [

	self subclassResponsibility 
]
