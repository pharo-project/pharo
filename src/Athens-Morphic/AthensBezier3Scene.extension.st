Extension { #name : #AthensBezier3Scene }

{ #category : #'*Athens-Morphic' }
AthensBezier3Scene class >> openViewOn: aControlPointsArray [

	^ AthensSceneView new
		scene: 	(self on: aControlPointsArray );
		openInWindow
]
