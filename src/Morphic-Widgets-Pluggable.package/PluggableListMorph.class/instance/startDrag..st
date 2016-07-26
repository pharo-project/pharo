drag and drop
startDrag: evt 
	| transferMorph draggedItem passenger |

	dragItemSelector ifNotNil: [ ^self startDragExtended: evt ].
	evt hand hasSubmorphs ifTrue: [^ self]. 
	self dragEnabled ifFalse: [^ self].
	"Here I ensure at least one element is selected "
	ActiveHand anyButtonPressed ifFalse: [ ^self ].
			
	draggedItem := self getListItem: (self mouseDownRow ifNil: [ self lastNonZeroIndex ]).
	draggedItem ifNil: [ ^ self ].
	passenger := self model dragPassengersFor: draggedItem inMorph: self.
	passenger ifNil: [ ^ self ].
		
	transferMorph := self model transferFor: passenger from: self.
	transferMorph align: transferMorph draggedMorph bottomLeft  with: evt position.
	
	self mouseDownRow: nil.
	transferMorph dragTransferType: (self model dragTransferTypeForMorph: self).
	
	[evt hand grabMorph: transferMorph ] 
		ensure: [
			Cursor normal show.
			evt hand releaseMouseFocus: self] 