drag and drop
wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM
	"We are only interested in TransferMorphs as wrappers for             
	informations. If their content is really interesting for us, will             
	determined later in >>acceptDroppingMorph:event:."

	| srcType dstType |
	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: AbstractWidget listClass)
		ifFalse: [ ^ false ].
	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.
	srcType == #getMethodItem:
		ifTrue:
			[ ^ dstType == #getCategoryItem: or: [ dstType == #getClassItem: ] ].
	srcType == #getCategoryItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	(srcType == #getClassItem: and: [ dstType = #getPackageItem: ])
		ifTrue:
			[ 
			(destinationLM potentialDropItem isKindOf: AbstractWidget listClass)
				ifTrue: [ ^ true ] ]
		ifFalse: [ ^ false ].
	^ false