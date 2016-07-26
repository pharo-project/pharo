drag and drop
wantsDroppedMorph: aMorph event: anEvent
	"I need to more or less copy this from old pluggable list because there 
	 is much bad code around :("

	^ aMorph dragTransferType == #dragTransfer 
		ifTrue: [ super wantsDroppedMorph: aMorph event: anEvent ]
		ifFalse: [ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self ]