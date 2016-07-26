loading
loadWithNameLike: aString
	" this is THE central entrypoint for all loading (gopher, metacello, monticello, ...) "
	^ self announceLoad: aString do: [ self basicLoadWithNameLike: aString ]