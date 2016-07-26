morphic ui
buttonSpecs
	^ #(('Add' addDependency 'Add a dependency')
		('Update' updateMenu 'Update dependencies')
		('Install' installMenu 'Load/Merge/Upgrade into image')
		('Up' up 'Move item up in list' canMoveUp)
		('Down' down 'Move item down in list' canMoveDown)
		('Remove' remove 'Remove item' canRemove)
		('Migrate' migrate 'Migrate all packages to a repository')
		('Store' store 'store configuration')
		)