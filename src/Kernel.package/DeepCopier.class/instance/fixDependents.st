like fullCopy
fixDependents
	"They are not used much, but need to be right"

	DependentsFields associationsDo:
		[:pair |
		pair value do:
			[:dep |
			(references at: dep ifAbsent: [nil]) ifNotNil:
				[:newDep| | newModel |
				newModel := references at: pair key ifAbsent: [pair key].
				newModel addDependent: newDep]]].
