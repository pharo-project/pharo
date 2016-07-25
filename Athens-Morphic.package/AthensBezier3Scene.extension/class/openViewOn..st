*Athens-Morphic
openViewOn: aControlPointsArray
	
	^ AthensSceneView new
		scene: 	(self on: aControlPointsArray );
		openInWindow