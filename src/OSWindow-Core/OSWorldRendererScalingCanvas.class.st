Class {
	#name : 'OSWorldRendererScalingCanvas',
	#superclass : 'OSWorldRenderer',
	#classInstVars : [
		'isApplicable'
	],
	#category : 'OSWindow-Core-Morphic',
	#package : 'OSWindow-Core',
	#tag : 'Morphic'
}

{ #category : 'examples' }
OSWorldRendererScalingCanvas class >> example [

	| world |

	(world := OSWindowWorldMorph new)
		scaleFactor: 0.5.
	world worldState worldRenderer: ((self forWorld: world)
		windowCloseAction: [ world osWindowCloseButtonPressed ]).
	world changed.
	world class addExtraWorld: world
]

{ #category : 'accessing' }
OSWorldRendererScalingCanvas class >> isApplicable [

	^ isApplicable ifNil: [ false ]
]

{ #category : 'accessing' }
OSWorldRendererScalingCanvas class >> isApplicable: boolean [

	isApplicable := boolean
]

{ #category : 'accessing' }
OSWorldRendererScalingCanvas class >> isApplicableFor: aWorld [

	^ self isApplicable
]

{ #category : 'utilities' }
OSWorldRendererScalingCanvas >> convertWindowMouseEventPosition: aPosition [

	^ super convertWindowMouseEventPosition: aPosition * world scaleFactor
]

{ #category : 'private' }
OSWorldRendererScalingCanvas >> doActivate [

	super doActivate.
	self osWindow backendWindow renderer canvasBlock: [ :formCanvas |
		| scale |
		scale := world scaleFactor reciprocal.
		(scale ~= 1 and: [ scale truncated = scale ])
			ifTrue: [ ScalingCanvas formCanvas: formCanvas scale: scale ]
			ifFalse: [ formCanvas ] ]
]

{ #category : 'operations' }
OSWorldRendererScalingCanvas >> updateDamage: allDamage [

	| scale |
	
	scale := world scaleFactor reciprocal.
	super updateDamage: (allDamage collect: [ :damageRect | damageRect scaleBy: scale ])
]

{ #category : 'display box access' }
OSWorldRendererScalingCanvas >> viewBox [

	^ 0@0 corner: ((self windowExtent / self windowScaleFactor) ceiling max: 1@1)
]
