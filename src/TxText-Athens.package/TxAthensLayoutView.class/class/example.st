examples
example
	"Open and then zoom in and out"
	
	| sceneView view |
	sceneView := AthensSceneView new.
	view := TxAthensLayoutView on: Morph comment asTxModel extent: 400 @ 400.

	sceneView scene: view.
	sceneView openInWindow