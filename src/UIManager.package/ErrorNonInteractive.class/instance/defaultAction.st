exceptiondescription
defaultAction
	" log error and quit "
	
	^ UIManager default 
		quitFrom: self signalerContext 
		withMessage: self description