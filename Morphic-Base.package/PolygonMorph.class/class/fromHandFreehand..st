instance creation
fromHandFreehand: hand
	"Let the user draw a polygon, holding the mouse down, and ending by clicking within 5 of the first point..."
	"self fromHandFreehand: ActiveHand"
	
	| p1 poly pN opposite |
	"wait till guy will press the mouse button"
	hand captureEventsUntil: [:evt |
		evt isMouse ifTrue: [ p1 := evt cursorPoint].
		(evt isMouse and: [ evt anyButtonPressed ])  ].
	
	opposite := (Display colorAt: p1) negated.
	opposite = Color transparent ifTrue: [opposite := Color red].
	(poly := LineMorph from: p1 to: p1 color: opposite width: 2) openInWorld.
	
	hand captureEventsWhile: [:evt |
		
		evt isMouse ifTrue:[ 
			pN := evt cursorPoint.
			(pN dist: poly vertices last) > 3 ifTrue:
				[poly setVertices: (poly vertices copyWith: pN) ].
			
			evt anyButtonPressed 
			]
		ifFalse: [ true ]
	].
	^ (poly setVertices: (poly vertices copyWith: p1)) delete
