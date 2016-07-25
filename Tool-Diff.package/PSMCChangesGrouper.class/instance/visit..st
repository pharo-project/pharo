grouping
visit: aMCOperation

	| previous |
	previous := currentOperation.
	currentOperation := aMCOperation.
	currentOperation definition accept: self.
	currentOperation := previous.