example
example
	| example |
	example := ClassMethodBrowser new.
	example
		classes: Smalltalk allClasses;
		openWithSpec.
		^example