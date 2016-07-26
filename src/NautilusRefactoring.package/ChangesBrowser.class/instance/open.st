opening
open
	^ self openDialogWithSpec
		okAction: [ self accept ];
		cancelAction: [ self cancel ];
		yourself