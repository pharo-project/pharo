accessing
statements: statements0 
	statements := statements0.
	statements do: [:statement | statement parent: self]