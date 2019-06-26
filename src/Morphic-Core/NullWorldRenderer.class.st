Class {
	#name : #NullWorldRenderer,
	#superclass : #AbstractWorldRenderer,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
NullWorldRenderer class >> isApplicableFor: aWorld [

	"This worldRenderer is always applicable"
	^ true
]

{ #category : #accessing }
NullWorldRenderer class >> priority [ 

	^ 0
]

{ #category : #activation }
NullWorldRenderer >> activate [
]

{ #category : #operations }
NullWorldRenderer >> activateCursor: aCursor withMask: maskForm [

	"We don't activate nothing"
]

{ #category : #accessing }
NullWorldRenderer >> actualScreenSize [

	^ 240@120
]

{ #category : #accessing }
NullWorldRenderer >> canvas [

	^ nil
]

{ #category : #accessing }
NullWorldRenderer >> canvas: x [ 

]

{ #category : #'display box access' }
NullWorldRenderer >> checkForNewScreenSize [

	"Do nothing"
]

{ #category : #activation }
NullWorldRenderer >> deactivate [
]

{ #category : #operations }
NullWorldRenderer >> deferUpdates: aValue [

	^ aValue value
]

{ #category : #operations }
NullWorldRenderer >> displayWorldState: aWorldState ofWorld: aWorld submorphs: submorphs [

	"Do Nothing"
]

{ #category : #operations }
NullWorldRenderer >> doDeferredUpdatingFor: aWorld [

	"Do Nothing"
]

{ #category : #operations }
NullWorldRenderer >> forceDamageToScreen: allDamage [

	"Do Nothing"
	
]

{ #category : #operations }
NullWorldRenderer >> forceDisplayUpdate [

	" Do Nothing "
]

{ #category : #'display box access' }
NullWorldRenderer >> usableArea [

	^ self actualScreenSize
]

{ #category : #'display box access' }
NullWorldRenderer >> viewBox [ 

	^ self actualScreenSize
]
