instance creation
createDrawingProcess
	^ [ 
	[ 
	(Delay forMilliseconds: 1000 / self maxFPS) wait.
	self redrawAndUpdateSurface ] repeat ] fork