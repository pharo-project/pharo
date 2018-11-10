Extension { #name : #CmdCommandActivator }

{ #category : #'*Iceberg-TipUI' }
CmdCommandActivator >> buildIceToolbarItem [
	| item |

	item := (IceTipToolbarItem new
		label: self activationStrategy toolbarName;
		icon: self menuItemIcon;
		position: self activationStrategy position;
		help: self activationStrategy help;
		badge: self command badge;
		action: [ self executeCommand ];
		yourself).
	
	^ item
]
