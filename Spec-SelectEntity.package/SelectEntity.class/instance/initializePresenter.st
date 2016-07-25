initialization
initializePresenter
	selectEntity 
		action: [  |select|
			select := self selectDialog value.
			select openDialogWithSpec
				okAction: [  
					self entity: select selectedItem ]]
