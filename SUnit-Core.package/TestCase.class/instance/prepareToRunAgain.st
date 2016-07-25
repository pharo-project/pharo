running
prepareToRunAgain 	

	"Used by the debugger when a test is restarted
	I'm not 'hidding'' possible exceptions because I think it is best to popup a new debugger
	if some one occurs and let the programmer decide what to do - Hernan"
	self 
		tearDown;
		setUp