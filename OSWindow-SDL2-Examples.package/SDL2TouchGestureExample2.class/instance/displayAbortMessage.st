drawing
displayAbortMessage
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: self windowCenter x - 30 @ self windowCenter y.
			canvas
				setPaint: Color green;
				setFont: ((LogicalFont familyName: 'Arial' pointSize: 1) emphasis: 1);
				setShape: (renderer athensSurface createPath: [ :builder | builder close ]);
				drawString: 'Swipe Aborted!';
				draw ]