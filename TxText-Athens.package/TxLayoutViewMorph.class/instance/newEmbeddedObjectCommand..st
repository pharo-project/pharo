factory
newEmbeddedObjectCommand: aTxEmbeddedObjectSpan 
	| morph |
	morph := TxWrappingMorph for: (aTxEmbeddedObjectSpan object asMorph).
	self registerEmbeddedMorph: morph.
	
	^ TxEmbeddedObjectCommand new 
		span: aTxEmbeddedObjectSpan;
		morph: morph;
		yourself