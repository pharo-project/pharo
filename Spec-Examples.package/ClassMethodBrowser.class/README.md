A ClassMethodBrowser is a simple browser using spec and reusing MethodBrowser to browse classes>>methods>>sourceCode.
	| cb |
	cb := ClassMethodBrowser new.
	cb openWithSpec.
	cb classes: Smalltalk allClasses.