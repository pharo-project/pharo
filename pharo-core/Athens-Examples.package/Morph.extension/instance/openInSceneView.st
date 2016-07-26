*Athens-Examples
openInSceneView
	^ AthensSceneView new
		scene: (AthensMorphScene new morph:self);
		openInWindow