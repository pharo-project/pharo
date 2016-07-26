instance creation
openOn: root extentBlock: extBlock childsBlock: childsBlock

	| scene |
	
	scene := self new root: root extentBlock: extBlock childsBlock: childsBlock.
		

	^ AthensSceneView new
		scene: scene;
		openInWindow.