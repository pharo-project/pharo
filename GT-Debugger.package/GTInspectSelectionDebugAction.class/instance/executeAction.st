actions
executeAction
	
	self codePresentation evaluateSelectionAndDo: [ :result |
		shouldBasicInspect 
			ifTrue: [ result basicInspect ]
			ifFalse: [ result inspect ] ]
		
