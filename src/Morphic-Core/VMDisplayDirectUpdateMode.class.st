Class {
	#name : #VMDisplayDirectUpdateMode,
	#superclass : #Object,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #'as yet unclassified' }
VMDisplayDirectUpdateMode >> updateDamage: damage ofWorld: aWorld onRenderer: aRenderer [

	aRenderer canvas
		showAt: aWorld viewBox origin
		invalidRects: damage
]
