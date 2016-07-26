windows
openRemoveDialogOn: aGroup
	^ UIManager default confirm: 'Do you really want to delete the group named ' , aGroup name , ' ?'