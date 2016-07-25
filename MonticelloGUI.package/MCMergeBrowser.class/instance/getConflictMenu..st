actions
getConflictMenu: aMenu
	selection remoteChosen
		ifTrue: [aMenu add: 'Undo keep change' target: self selector: #clearChoice]
		ifFalse: [aMenu add: 'Keep change' target: self selector: #chooseRemote].
	selection localChosen
		ifTrue: [aMenu add: 'Undo reject change' target: self selector: #clearChoice]	
		ifFalse: [aMenu add: 'Reject change' target: self selector: #chooseLocal].
	^ aMenu