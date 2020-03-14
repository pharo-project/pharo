Class {
	#name : #VMDisplayDeferredUpdateMode,
	#superclass : #Object,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #'as yet unclassified' }
VMDisplayDeferredUpdateMode >> updateDamage: damage ofWorld: aWorld onRenderer: renderer [

	renderer forceDamageToScreen: damage
]

{ #category : #'as yet unclassified' }
VMDisplayDeferredUpdateMode >> updateDamage: damage onRenderer: renderer [

	renderer forceDamageToScreen: damage
]
