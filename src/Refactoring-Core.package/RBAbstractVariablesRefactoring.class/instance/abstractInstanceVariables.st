transforming
abstractInstanceVariables
	| variables |
	(instVarReaders isEmpty and: [ instVarWriters isEmpty ])
		ifTrue: [ ^ self].
	variables := Set new.
	variables
		addAll: instVarReaders;
		addAll: instVarWriters.
	variables do: [ :each | self abstractInstanceVariable: each ]