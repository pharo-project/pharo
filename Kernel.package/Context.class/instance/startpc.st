private
startpc
	^closureOrNil
		ifNil:	[self method initialPC]
		ifNotNil: [closureOrNil startpc]