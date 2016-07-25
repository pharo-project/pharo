utilities
measureSelectionTimeInFinderForSize: size
	"
	self new measureSelectionTimeInFinderForSize: 20 
	"
	| browser time window |

	browser := self defaultFinderBrowser.
	window := browser openOn: size.
	
	time := Time millisecondsToRun: 
			[size timesRepeat: [ (window model panes last port: #selection) value: size].].
		
	window delete.
	^ {size . time}