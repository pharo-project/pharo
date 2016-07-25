initialization
removeActionsForSession: aSession

	aSession ifNotNil: [ 
		aSession  
			removeActionsForEvent: #restart;
			removeActionsForEvent: #resume;
			removeActionsForEvent: #stepInto;
			removeActionsForEvent: #stepOver;
			removeActionsForEvent: #stepThrough;
			removeActionsForEvent:#contextChanged ]. 