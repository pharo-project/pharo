as yet unclassified
actOnContextChanged: ann 
	ann property = #selection ifTrue: [ 
		listModel announcer suspendAllWhile: [
			dropDownMorph listSelectionIndex: (listModel glamourPresentation displayValue indexOf: ann value) ] ]