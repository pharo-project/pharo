*Morphic-Base
addWorldHandlesTo: aHaloMorph box: box
	aHaloMorph haloBox: box.
	HaloMorph haloSpecificationsForWorld 
		do: [:aSpec | aHaloMorph perform: aSpec addHandleSelector with: aSpec].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box