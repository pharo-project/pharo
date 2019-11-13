"
I am a null renderer.
I am used when we are in command line or during the initialization
"
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

	| canvas worldDamageRects handsToDraw |

	canvas := (Form extent: self actualScreenSize depth: 32) getCanvas.

	submorphs do: [ :m | m fullBounds ].	

	aWorldState checkIfUpdateNeeded ifFalse: [^ self].  

	worldDamageRects := aWorldState drawWorld: aWorld submorphs: submorphs invalidAreasOn: canvas.  
	"repair world's damage on canvas"

	handsToDraw := aWorldState selectHandsToDrawForDamage: worldDamageRects.
	handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"
 
	canvas.

]

{ #category : #activation }
NullWorldRenderer >> doActivate [
]

{ #category : #operations }
NullWorldRenderer >> forceDamageToScreen: allDamage [

	"Do Nothing"
	
]

{ #category : #operations }
NullWorldRenderer >> forceDisplayUpdate [

	" Do Nothing "
]

{ #category : #accessing }
NullWorldRenderer >> icon: aForm [ 

	"Do nothing, a null world rendered has no window, so no icon either"
]
