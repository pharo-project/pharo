"
I am a strategy managing how updates are pushed to the screen when changes are deferred.
"
Class {
	#name : #VMDisplayDeferredUpdateMode,
	#superclass : #Object,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #rendering }
VMDisplayDeferredUpdateMode >> updateDamage: damage ofWorld: aWorld onRenderer: renderer [

	renderer forceDamageToScreen: damage
]
