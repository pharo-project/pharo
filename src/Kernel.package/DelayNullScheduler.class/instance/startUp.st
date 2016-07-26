snapshotting
startUp
	self error: 'Delay scheduler is NOT RUNNING!'.
	"The running delay scheduler seems to be DelayNullScheduler.
	 This is probably not what you want. See Tools > Process Browser.
	 
	 If all is well, the following should result in a value 1 second...
	    [1 second asDelay wait] timeToRun

	 To get delays working again, go to System > Settings > System > Delay scheduler.	
	 "
 