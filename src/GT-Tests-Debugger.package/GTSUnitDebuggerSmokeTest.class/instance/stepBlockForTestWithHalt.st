utils
stepBlockForTestWithHalt

	^ [ :aProcess |
		aProcess 
			"Step into the execution of the test method"
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;	
			step;
		
			"Step until before the message send #halt"
			stepToSendOrReturn;
			completeStep: aProcess suspendedContext;
			stepToSendOrReturn ]
		
