actions
revertChangeAction
	^ GLMGenericAction new
		action: [ :presentation | self entity revertChange: presentation selection ];
		condition: [ self entity canRevertChanges ];
		icon: #glamorousUndo asIcon;
		title: 'Revert change'; 
		showTitle