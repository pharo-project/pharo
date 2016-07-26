unknown-var
createVariableCorrection
	"	<undeclaredVarCommand>"

	"Unable because it's not fully implemented"

	| variables index |
	^ SugsSuggestion
		for: [ :context | 
			variables := context selectedClass allInstVarNames asArray sort.
			index := UIManager default chooseFrom: variables.
			UIManager default inform: (variables at: index) ]
		named: 'Correct variable to...' translated
		icon: (Smalltalk ui icons iconNamed: #smallFindIcon)