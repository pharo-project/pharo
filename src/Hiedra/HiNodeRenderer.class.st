"
I know how to render a ruler node in an Athens canvas.

Note: an instance of me cannot be used to render in more than one canvas.
"
Class {
	#name : #HiNodeRenderer,
	#superclass : #HiRenderer,
	#instVars : [
		'location',
		'nodeShape',
		'nodeBuilder'
	],
	#category : #Hiedra-Rendering
}

{ #category : #initialization }
HiNodeRenderer >> initializeShape [
	nodeShape := athensCanvas createPath: [ :builder | 
		builder
			absolute;
			moveTo: -1 @ 0;
			ccwArcTo: 0 @ 1 angle: 90 degreesToRadians;
			ccwArcTo: 1 @ 0 angle: 90 degreesToRadians;
			ccwArcTo: 0 @ -1 angle: 90 degreesToRadians;
			ccwArcTo: -1 @ 0 angle: 90 degreesToRadians ]


]

{ #category : #accessing }
HiNodeRenderer >> location: aPoint [ 
	location := aPoint
]

{ #category : #accessing }
HiNodeRenderer >> nodeBuilder: aNodeBuilder [ 
	nodeBuilder := aNodeBuilder
]

{ #category : #accessing }
HiNodeRenderer >> nodeModel: aNodeBuilder [ 
	self 
		deprecated: 'Use nodeBuilder: instead'
		on: 	'17 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self nodeBuilder: aNodeBuilder 
]

{ #category : #rendering }
HiNodeRenderer >> render [
	"Render this node in the athensCanvas"

	self initializeShape.

	athensCanvas paintMode restoreAfter: [
		athensCanvas pathTransform restoreAfter: [ 
			athensCanvas pathTransform
				translateX: location x
				Y: location y.

			athensCanvas pathTransform restoreAfter: [ 
				athensCanvas pathTransform scaleBy: nodeBuilder width.
				athensCanvas setPaint: nodeBuilder color.
				athensCanvas drawShape: nodeShape ].
			
			athensCanvas pathTransform scaleBy: nodeBuilder width * 0.65.
			athensCanvas paintMode clear.
			athensCanvas drawShape: nodeShape ]]
]
