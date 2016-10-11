instance creation
fromHand: hand
	"Let the user draw a polygon, clicking at each vertex, and ending by clicking within 5 of the first point..."
	"self fromHand: ActiveHand"
	
	| p1 poly oldVerts pN opposite |
	"wait till guy will press the mouse button"
	hand captureEventsUntil: [:evt |
		evt isMouse ifTrue: [ p1 := evt cursorPoint].
		(evt isMouse and: [ evt anyButtonPressed ]) not ].
	
	opposite := (Display colorAt: p1) negated.
	opposite = Color transparent ifTrue: [opposite := Color red].
	(poly := LineMorph from: p1 to: p1 color: opposite width: 2) openInWorld.
	oldVerts := {p1}.
	
	[true] whileTrue: [
		"wait till button release"
		hand captureEventsWhile: [:evt |
			evt isMouse 
				ifTrue: [ 
					pN := evt cursorPoint.
					poly setVertices: (oldVerts copyWith: pN).
					evt anyButtonPressed ] 
				ifFalse: [ true ] ].

		(oldVerts size > 1 and: [(pN distanceTo: p1) < 5]) 
			ifFalse: [
				oldVerts := poly vertices.
				"loop till button will be pressed"
				hand captureEventsUntil: [:evt | 		
					evt isMouse 
						ifTrue: [ 
							pN := evt cursorPoint.
							poly setVertices: (oldVerts copyWith: pN).
							evt anyButtonPressed ] 
					ifFalse: [ false ]]]
			ifTrue: [ ^ (poly setVertices: (poly vertices copyWith: p1)) delete].
	]