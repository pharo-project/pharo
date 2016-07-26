drag and drop
startDrag: event
	| passengers transferMorph |

	event hand hasSubmorphs ifTrue: [^ self]. 
	self dragEnabled ifFalse: [^ self].
	"Here I ensure at least one element is selected "
	event hand anyButtonPressed ifFalse: [ ^self ].
	self hasSelection ifFalse: [ ^ self ].
			
	passengers := self selectedRowIndexes collect: [ :each | self dataSource passengerAt: each ].
	transferMorph := self dataSource transferFor: passengers from: self.
	transferMorph align: transferMorph draggedMorph bottomLeft with: event position.
	transferMorph dragTransferType: self dataSource dragTransferType.
	
	event hand attachMorph: transferMorph 