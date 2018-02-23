Extension { #name : #Object }

{ #category : #'*Morphic-Widgets-Basic' }
Object >> acceptDroppingMorph: transferMorph event: evt inMorph: dstListMorph [ 
	
	^false.
]

{ #category : #'*Morphic-Widgets-Basic' }
Object >> dragPassengerFor: item inMorph: dragSource [ 
	^item
]

{ #category : #'*Morphic-Widgets-Basic' }
Object >> dragTransferType [
	^nil
]

{ #category : #'*Morphic-Widgets-Basic' }
Object >> dragTransferTypeForMorph: dragSource [ 
	^nil
]

{ #category : #'*Morphic-Widgets-Basic' }
Object >> wantsDroppedMorph: aMorph event: anEvent inMorph: destinationLM [ 
	^false 
]

{ #category : #'*Morphic-Widgets-Basic' }
Object >> withoutListWrapper [

	^self
]
