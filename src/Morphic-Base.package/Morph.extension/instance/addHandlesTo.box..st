*Morphic-Base
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	
	aHaloMorph haloBox: box.
	HaloMorph  currentHaloSpecifications do:
		[:aSpec | | wantsIt aSelector | 
			aSelector := aSpec addHandleSelector.
			(wantsIt := self wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph)
				ifTrue: [(#(addDupHandle:) includes: aSelector) 
					ifTrue: [wantsIt := self preferredDuplicationHandleSelector = aSelector]].
			wantsIt 
				ifTrue: [aHaloMorph perform: aSelector with: aSpec]].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box