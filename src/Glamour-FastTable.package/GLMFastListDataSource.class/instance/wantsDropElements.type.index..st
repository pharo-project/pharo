drag and drop
wantsDropElements: aCollection type: type index: rowIndex
	^ (aCollection isEmptyOrNil or: [ type ~= self dragTransferType ])
		ifFalse: [ self glamourPresentation allow: (self collectionToDragPassenger: aCollection) droppedOnItem: (self elementOrZeroAt: rowIndex) ]
		ifTrue: [ false ]