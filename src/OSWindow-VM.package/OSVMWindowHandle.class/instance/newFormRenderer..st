instance-creation
newFormRenderer: form 
	^ renderer := OSVMFormRenderer new 
		form: form;
		window: self;
		yourself