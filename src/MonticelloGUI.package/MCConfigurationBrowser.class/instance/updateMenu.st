actions
updateMenu

	| menu |
	menu := UIManager default
				 newMenuIn: self for: self.
	menu add: 'update from image' selector: #updateFromImage.
	menu add: 'update from repositories' selector: #updateFromRepositories.
	menu popUpInWorld.