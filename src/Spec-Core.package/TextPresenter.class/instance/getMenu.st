private
getMenu
	^ (PragmaMenuBuilder
		pragmaKeyword: self textArea editingModeClass menuKeyword
		model: self widget) menu