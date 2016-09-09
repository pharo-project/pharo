accessing
model: aNautilusUI
	model := aNautilusUI.
	environment := (RBClassModelFactory rbNamespace onEnvironment: aNautilusUI browsedEnvironment)
						name: self printString;
						yourself