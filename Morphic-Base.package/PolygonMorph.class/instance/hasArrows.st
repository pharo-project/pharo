testing
hasArrows
	"Are all the conditions meet for having arrows?"
	^ (closed or: [arrows == #none or: [vertices size < 2]]) not