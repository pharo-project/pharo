Extension { #name : #CmdExtraMenuGroup }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdExtraMenuGroup >> buildContextMenu: aMenu [
	aMenu addLine.
	
	super buildContextMenu: aMenu
]
