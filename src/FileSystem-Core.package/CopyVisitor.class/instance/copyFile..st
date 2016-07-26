visiting
copyFile: aReference
	| copy |
	copy := dest resolve: (aReference relativeTo: source).
	aReference copyTo: copy