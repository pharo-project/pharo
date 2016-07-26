testing
isCurvy
	"Test for significant curves.  
	Small smoothcurves in practice are straight."
	^ smoothCurve and: [vertices size > 2]