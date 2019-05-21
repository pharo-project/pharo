Extension { #name : #CmdMenuGroup }

{ #category : #'*Spec-Commands' }
CmdMenuGroup >> inlineContextMenuItemsInto: aMenu [
	self flag: #pharoFixMe. "This is an ugly hack because this part of Spec is still in development. Later this should be removed."
	(aMenu isKindOf: Morph)
		ifTrue: [ aMenu addLine.
			contents do: [ :each | each buildContextMenu: aMenu ].
			aMenu addLine ]
		ifFalse: [ aMenu addGroup: [ :group | contents do: [ :each | each buildContextMenu: group ] ] ]
]
