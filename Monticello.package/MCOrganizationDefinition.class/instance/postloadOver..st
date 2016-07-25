installing
postloadOver: oldDefinition
	SystemOrganization categories:
		(self
			reorderCategories: SystemOrganization categories
			original: (oldDefinition ifNil: [#()] ifNotNil: [oldDefinition categories]))