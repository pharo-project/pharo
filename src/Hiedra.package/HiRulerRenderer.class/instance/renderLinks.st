private
renderLinks
	self flag: #todo. "FIX"

	ruler links do: [ :link |
		link linkBuilder renderer
			rulerBuilder: rulerBuilder;
			athensCanvas: athensCanvas;
			checkpoints: (self canvasPointsForAll: link checkpoints);
			render ]