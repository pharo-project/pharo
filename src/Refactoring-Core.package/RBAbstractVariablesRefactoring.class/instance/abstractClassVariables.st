transforming
abstractClassVariables
	| variables |
	(classVarReaders isEmpty and: [ classVarWriters isEmpty ])
		ifTrue: [ ^ self ].
	variables := Set new.
	variables
		addAll: classVarReaders;
		addAll: classVarWriters.
	variables do: [ :each | self abstractClassVariable: each ]