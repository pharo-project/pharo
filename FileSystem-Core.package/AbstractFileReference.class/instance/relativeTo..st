navigating
relativeTo: landmark
	"Answer a new path relative to landmark."
	
	"parent/child/grandChild relativeTo: parent returns child/grandChild
	(Filesystem disk / 'griffle' / 'plonk' / 'nurp') relativeTo: (Filesystem disk / 'griffle') 
	returns plonk/nurp"

	^ landmark makeRelative: self resolve