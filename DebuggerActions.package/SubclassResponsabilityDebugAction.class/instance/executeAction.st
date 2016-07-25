actions
executeAction
	| senderContext msg msgCategory chosenClass |
	
	senderContext := self interruptedContext sender.
	msg := Message 
		selector: senderContext selector
		arguments: senderContext arguments.
	msgCategory := senderContext methodClass organization categoryOfElement: msg selector.
	
	chosenClass := self 
		askForSuperclassOf: senderContext receiver class 
		to: senderContext methodClass 
		toImplement: senderContext selector 
		ifCancel: [ ^ self ].

	self session 
		implement: msg 
		classified: msgCategory 
		inClass: chosenClass 
		forContext: senderContext.