events
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph"
	^ aMorph isWorldMorph or: [self embeddable]