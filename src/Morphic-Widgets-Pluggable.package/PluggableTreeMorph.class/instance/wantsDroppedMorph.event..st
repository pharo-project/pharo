morphic
wantsDroppedMorph: aMorph event: anEvent
	
	aMorph dragTransferType == #dragTransfer ifFalse: [ ^ false ].
	
	^ wantsDropSelector 
		ifNil: [ model
			wantsDroppedMorph: aMorph 
			event: anEvent 
			inMorph: self ]
		ifNotNil: [
			model 
				perform: wantsDropSelector 
				with: aMorph passenger ]