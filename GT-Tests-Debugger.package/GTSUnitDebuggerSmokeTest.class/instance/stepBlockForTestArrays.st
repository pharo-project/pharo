utils
stepBlockForTestArrays

	^ [ :aProcess |
		aProcess
			"Step into the execution of the test method"
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;	
			step;
			
			"Step into the assert method"
			completeStep: aProcess suspendedContext; 
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;
			step ]
		
