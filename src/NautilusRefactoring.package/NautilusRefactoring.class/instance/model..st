accessing
model: aNautilusUI
	model := aNautilusUI.
	environment := (RBNamespace onEnvironment: aNautilusUI browsedEnvironment)
						name: self printString;
						yourself