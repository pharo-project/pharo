helpers
checkpoint: name

	| isNewSession |
	isNewSession := Smalltalk saveAs: name.
	isNewSession ifTrue: [ self halt ]