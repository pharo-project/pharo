drag and drop
wantsDroppedMorph: aMorph event: anEvent
	^ aMorph dragTransferType == #dragTransfer 
		ifTrue: [ dropItemSelector ifNil: [^false].
			wantsDropSelector ifNil: [^true].
			(model perform: wantsDropSelector with: aMorph passenger)]
		ifFalse: [ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self]