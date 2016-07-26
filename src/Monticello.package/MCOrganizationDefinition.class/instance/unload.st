unloading
unload
	categories
		do: [ :category | 
			(SystemOrganization isEmptyCategoryNamed: category)
				ifTrue: [ SystemOrganization removeCategory: category ] ]