dropping/grabbing
repelsMorph: aMorph event: ev
	(aMorph wantsToBeDroppedInto: self) ifFalse: [^ false].
	self dropEnabled ifFalse: [^ true].
	(self wantsDroppedMorph: aMorph event: ev) ifFalse: [^ true].
	^ super repelsMorph: aMorph event: ev "consults #repelling flag"