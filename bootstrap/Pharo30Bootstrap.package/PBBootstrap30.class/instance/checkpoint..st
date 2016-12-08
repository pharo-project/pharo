helpers
checkpoint: name

	| isNewSession |
	isNewSession := Smalltalk saveAs: name.
	isNewSession ifTrue: [ UIManager default defer: [ self halt ]]