accessing
promptForCancel
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := UIManager default
		confirm: 'Contents has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil.
	choice
		ifNotNil: [ 
			choice
				ifTrue: [ 
					self acceptTextInModel.
					self basicHasUnacceptedEdits: false ]
				ifFalse: [ self model 
						ifNotNil: [ :m | m clearUserEditFlag ] 
						ifNil: [ self basicHasUnacceptedEdits: false ] ] ]