api
during: aBlock
	<debuggerCompleteToSender>
	self default 
		stopMethod: thisContext sender method 
		during: aBlock