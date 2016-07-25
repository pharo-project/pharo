styling
promptForCancel
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := ( UIManager default
		confirm: 'Code has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil ).
	choice ifNotNil: [
		choice
			ifTrue: [ self accept ]
			ifFalse: [ self model clearUserEditFlag ]]