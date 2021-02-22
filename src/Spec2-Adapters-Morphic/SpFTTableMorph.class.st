Class {
	#name : #SpFTTableMorph,
	#superclass : #FTTableMorph,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'drag and drop' }
SpFTTableMorph >> acceptDroppingMorph: aTransferMorph event: anEvent [

	^ self dataSource model adapter
		acceptDroppingMorph: aTransferMorph 
		event: anEvent
]

{ #category : #'as yet unclassified' }
SpFTTableMorph >> wantsDroppedMorph: aTransferMorph event: anEvent [

	aTransferMorph isTransferable ifFalse: [ ^ false ].
	^ self dataSource model adapter 
		wantsDroppedMorph: aTransferMorph 
		event: anEvent
]
