menu
handleWindowClosed
	debugSession interruptedProcess == nil 
		ifFalse: [ 
			debugSession 
			resume: nil;
			clear ]