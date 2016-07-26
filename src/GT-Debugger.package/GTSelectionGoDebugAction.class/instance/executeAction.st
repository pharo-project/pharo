actions
executeAction
	
	self codePresentation 
		evaluateSelectionAndDo: [ :result | self debugger inspect: result ] 
		
