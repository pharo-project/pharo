Extension { #name : #CmdMenuCommandActivationStrategy }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuCommandActivationStrategy class >> buildContextMenuFor: aMorph inContext: aToolContext [

	| menu |
	menu := self buildMenuInContext: aToolContext.	
	^menu buildContextMenuFor: aMorph
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuCommandActivationStrategy class >> registerContextMenuOn: aBuilder inContext: aToolContext [
	| menu |
	menu := self buildMenuInContext: aToolContext.
	menu registerContextMenuItemsWithBuilder: aBuilder
]
