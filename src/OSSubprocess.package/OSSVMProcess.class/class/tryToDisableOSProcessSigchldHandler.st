OS Process
tryToDisableOSProcessSigchldHandler
	
	self flag: #fixIt. "hack!!!!"
	Smalltalk at: #OSProcess ifPresent: [ 
		(Smalltalk at: #OSProcess) thisOSProcess processAccessor restoreSigChld. 
		(Smalltalk at: #OSProcess) thisOSProcess processAccessor grimReaperProcess terminate.
		(Smalltalk at: #ThisOSProcess) shutDown: true.
		Smalltalk removeFromShutDownList: (Smalltalk at: #ThisOSProcess).
		Smalltalk removeFromStartUpList: (Smalltalk at: #ThisOSProcess).
	].