as yet unclassified
test
	| text view  sceneView sel |
	text := self newText.
	
	sel := text select all.

	sel applyAttribute: (TxFontAttribute with: (LogicalFont familyName: 'Arial'
 pointSize: 10)).

	sel end 
		moveToStart;
		moveRight: 22.
	sel
		applyAttribute: (TxFontAttribute with: (LogicalFont familyName: 'Tahoma'
 pointSize: 20)).
	view := nil.
	
	sceneView := AthensSceneView new scene:
		[ :canvas |
			view ifNil: [ 
				view := TxAthensLayoutView surface: canvas surface on:text extent:300@200.
				 ].
			view surface: canvas surface.
			view renderOn: canvas.
		 ].
	^ sceneView openInWindow.
