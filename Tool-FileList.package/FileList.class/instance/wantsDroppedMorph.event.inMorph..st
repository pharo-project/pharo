drag'n'drop
wantsDroppedMorph: aTransferMorph event: evt inMorph: dest
	| retval |
	retval := (aTransferMorph isTransferable)
		and: [ (aTransferMorph dragTransferType == #file)
				and: [ self isDirectoryList: dest ]].
	^retval