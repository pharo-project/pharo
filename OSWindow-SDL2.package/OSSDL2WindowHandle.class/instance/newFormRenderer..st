instance creation
newFormRenderer: form
	^ renderer := OSSDL2FormRenderer new 
		form: form;
		window: self;
		yourself