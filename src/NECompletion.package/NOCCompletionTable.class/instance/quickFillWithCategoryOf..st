protocol
quickFillWithCategoryOf: cls
	(Smalltalk organization classesInCategory: cls theNonMetaClass category) do: [:e | self quickFillWithClass: e]