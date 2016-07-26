execute
alertValidationErrors: errors for: configClassName
	| message |
	
	message :=  'There are critical issues in configuration ' , configClassName printString, '. Configuration not saved!' , Character cr asString.
	errors do: [ :error |
		 message := message , Character cr asString, error asString ]. 
	UIManager default 
		alert: message
		title: 'Critical Issues'