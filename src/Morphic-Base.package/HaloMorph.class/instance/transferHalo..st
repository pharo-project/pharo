events
transferHalo: event
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	target transferHalo: (event transformedBy: (target transformedFrom: self)) from: target.