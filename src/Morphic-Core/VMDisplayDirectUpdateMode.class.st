"
I am a strategy managing how updates are pushed to the screen when changes are NOT deferred.
"
Class {
	#name : #VMDisplayDirectUpdateMode,
	#superclass : #Object,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #rendering }
VMDisplayDirectUpdateMode >> updateDamage: damage ofWorld: aWorld onRenderer: aRenderer [

	aRenderer canvas
		showAt: aWorld viewBox origin
		invalidRects: damage
]
