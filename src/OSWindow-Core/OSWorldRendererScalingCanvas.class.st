Class {
	#name : #OSWorldRendererScalingCanvas,
	#superclass : #OSWorldRenderer,
	#category : #'OSWindow-Core-Morphic'
}

{ #category : #examples }
OSWorldRendererScalingCanvas class >> example [

	| world |

	(world := OSWindowWorldMorph new)
		scaleFactor: 0.5.
	world worldState worldRenderer: ((self forWorld: world)
		windowCloseAction: [ world osWindowCloseButtonPressed ]).
	world changed.
	world class addExtraWorld: world
]

{ #category : #accessing }
OSWorldRendererScalingCanvas class >> isApplicableFor: aWorld [

	^ false
]

{ #category : #utilities }
OSWorldRendererScalingCanvas >> convertWindowMouseEventPosition: aPosition [

	^ super convertWindowMouseEventPosition: aPosition * world scaleFactor
]

{ #category : #private }
OSWorldRendererScalingCanvas >> doActivate [

	super doActivate.
	self osWindow backendWindow renderer canvasBlock: [ :formCanvas |
		ScalingCanvas formCanvas: formCanvas scale: world scaleFactor reciprocal ]
]

{ #category : #operations }
OSWorldRendererScalingCanvas >> updateDamage: allDamage [

	| scale |
	
	scale := world scaleFactor reciprocal.
	super updateDamage: (allDamage collect: [ :damageRect | damageRect scaleBy: scale ])
]
