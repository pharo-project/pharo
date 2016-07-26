examples
examplePlaygroundWithBindings
	| playground wsBindings |
	wsBindings := Dictionary newFrom: { #number-> 1 }.
	playground := Smalltalk tools workspace
		openContents: 'number + 1'.
	playground
		setBindings: wsBindings.