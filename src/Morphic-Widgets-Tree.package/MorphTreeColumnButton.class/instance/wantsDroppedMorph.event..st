accessing
wantsDroppedMorph: aMorph event: anEvent 
	^ model container columnDropUnabled and: [(aMorph isTransferable) and: [(aMorph passenger isKindOf: MorphTreeColumn) and: [aMorph passenger ~= self model]]]