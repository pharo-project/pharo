creating patches
patchSequenceDoIfMatch: matchBlock ifInsert: insertBlock ifRemove: removeBlock
	"I'm the general purpose method to iterate through the patch sequence. See my senders to learn how to use me."

	| xLine xLineStream |
	xLineStream := xLines readStream.
	yLines
		do: [ :yLine | 
			yLine hasMatch
				ifFalse: [ insertBlock value: yLine string ]
				ifTrue: [ 
					[ (xLine := xLineStream next) isNil or: [ xLine == yLine match ] ] whileFalse: [ removeBlock value: xLine string ].
					matchBlock value: yLine string ] ].
	[ (xLine := xLineStream next) isNil ] whileFalse: [ removeBlock value: xLine string ]