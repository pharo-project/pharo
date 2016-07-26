drag and drop
wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM
	| srcType dstType |
	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: self class listClass)
		ifFalse: [ ^ false ].
	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.
	srcType == #getMethodItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	srcType == #getCategoryItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	^ false
	
