private
menu: aMenu shifted: aBoolean
	(aMenu add: 'Refresh' target: self selector: #collectAllShortcuts) 
	icon: (self iconNamed: #smallUpdateIcon).
	self kmTable selectedItem ifNil:[^ aMenu].
	(aMenu add: 'Browse Definition' target: self selector: #browseDefinitionOfSelectedShortcut)
	icon: (self iconNamed: #smallSystemBrowserIcon).
	(aMenu add: 'Inspect Action' target: self selector: #inspectActionOfSelectedShortcut)
	icon: (self iconNamed: #smallInspectItIcon).
	^ aMenu