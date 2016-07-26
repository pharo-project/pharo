public
allItemsIn: aContext

	^ allCandidates ifNil: [ allCandidates := (self computeAllItemsIn: aContext) copy ].