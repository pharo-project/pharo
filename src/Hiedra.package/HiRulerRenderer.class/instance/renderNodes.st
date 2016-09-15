private
renderNodes
	ruler nodes do: [ :aNode | 
		rulerBuilder nodeBuilder renderer
			athensCanvas: athensCanvas;
			location: (self canvasPointFor: aNode);
			render ]