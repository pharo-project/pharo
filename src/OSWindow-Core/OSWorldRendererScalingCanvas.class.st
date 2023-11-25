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

{ #category : 'private' }
OSWorldRendererScalingCanvas >> canvasScale [

	| scale |

	scale := world scaleFactor reciprocal.
	^ (scale ~= 1 and: [ scale truncated = scale ])
		ifTrue: [ scale ]
		ifFalse: [ nil ]
]

{ #category : 'utilities' }
OSWorldRendererScalingCanvas >> convertWindowMouseEventPosition: aPosition [

	^ self canvasScale
		ifNil: [ super convertWindowMouseEventPosition: aPosition ]
		ifNotNil: [ :canvasScale | super convertWindowMouseEventPosition: aPosition * canvasScale reciprocal ]
]

{ #category : 'private' }
OSWorldRendererScalingCanvas >> doActivate [

	super doActivate.
	self osWindow backendWindow renderer canvasBlock: [ :formCanvas |
		self canvasScale
			ifNotNil: [ :scale | ScalingCanvas formCanvas: formCanvas scale: scale ]
			ifNil: [ formCanvas ] ]
]

{ #category : 'operations' }
OSWorldRendererScalingCanvas >> updateDamage: allDamage [

	super updateDamage: (self canvasScale ifNil: [ allDamage ] ifNotNil: [ :canvasScale |
		allDamage collect: [ :damageRect | damageRect scaleBy: canvasScale ] ])
]

{ #category : 'display box access' }
OSWorldRendererScalingCanvas >> viewBox [

	self canvasScale ifNil: [ ^ super viewBox ].
	^ 0@0 corner: ((self windowExtent / self windowScaleFactor) ceiling max: 1@1)
]
