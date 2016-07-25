drag and drop
wantsDroppedMorph: transferMorph event: evt inMorph: listMorph
	(transferMorph isKindOf: TransferMorph)
		ifFalse: [ ^ false ].
	(self model groupManager dragTransferTypeForMorph: listMorph) = #SelectedPackage
		ifFalse: [ ^ false ].
	^ true