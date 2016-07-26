drag and drop
wantsDroppedMorph: aMorph event: anEvent inMorph: destMorph
	"TODO: Filter out to avoid drag and drop of different objet types. Here, we don't know what the target may be so filtering is limited."

	aMorph dragTransferType ifNil: [ ^ false ].
	^ destMorph model == self