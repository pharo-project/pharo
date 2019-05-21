Extension { #name : #CmdMenu }

{ #category : #'*Spec-Commands' }
CmdMenu >> buildContextMenu [
	| menu |

	menu := MenuPresenter new.
	rootGroup buildContextMenu: menu.
	^ menu
]
