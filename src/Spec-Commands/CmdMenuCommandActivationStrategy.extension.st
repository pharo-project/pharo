Extension { #name : #CmdMenuCommandActivationStrategy }

{ #category : #'*Spec-Commands' }
CmdMenuCommandActivationStrategy class >> buildContextKeyBindingsInContext: aToolContext [
	| bindings |

	bindings := Dictionary new.
	CmdShortcutActivation 
		visibleInstancesInContext: aToolContext
		do: [ :activation | 
			| activator |
			activator := activation newActivatorFor: aToolContext.
			bindings
				at: activator activationStrategy keyCombination
				put: [ activator executeCommand ] ].
	^ bindings
]

{ #category : #'*Spec-Commands' }
CmdMenuCommandActivationStrategy class >> buildContextMenuInContext: aToolContext [
	| menu |
	
	menu := self buildMenuInContext: aToolContext.
	^ menu buildContextMenu
]
