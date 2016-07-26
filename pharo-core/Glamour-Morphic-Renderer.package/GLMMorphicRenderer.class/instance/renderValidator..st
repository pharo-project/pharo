rendering browsers
renderValidator: aBrowser 

	| tmpValidatorRenderer |
	tmpValidatorRenderer :=  GLMMorphicValidatorRenderer new.
	aBrowser  setSpecificAnnouncementActionsTo: tmpValidatorRenderer.  
	^ tmpValidatorRenderer render: aBrowser from: self