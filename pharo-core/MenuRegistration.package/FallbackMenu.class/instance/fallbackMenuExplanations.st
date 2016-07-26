private
fallbackMenuExplanations

	| workspace wsBindings |
	wsBindings := Dictionary newFrom: { #menu-> self }.
	workspace := Smalltalk tools workspace
		openContents: '"Something is wrong with this menu. 
To investigate just debug following expression:"
menu debug'.
	workspace
		label: 'Fallback menu explanations';
		setBindings: wsBindings.