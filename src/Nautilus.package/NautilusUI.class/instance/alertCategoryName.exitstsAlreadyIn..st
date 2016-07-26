package
alertCategoryName: tagName exitstsAlreadyIn: package
	| errorMessage |
	errorMessage := String
		streamContents: [ :stream | stream << 'Category named ' << tagName << ' already exists in package ' << package name ].
	UIManager default inform: errorMessage