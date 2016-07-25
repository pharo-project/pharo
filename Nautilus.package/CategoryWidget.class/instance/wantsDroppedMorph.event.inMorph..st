drag and drop
wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM 
	"We are only interested in TransferMorphs as wrappers for             
	informations. If their content is really interesting for us, will             
	determined later in >>acceptDroppingMorph:event:."

	| srcType dstType |

	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: self class listClass) ifFalse: [ ^ false ].

	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.

	^ srcType == #getMethodItem: and: [ dstType == #getCategoryItem:]